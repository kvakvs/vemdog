%%% @author Dmytro Lytovchenko <dmytro.lytovchenko@gmail.com>
%%% @doc Stores a history of active trace in a public ETS table
-module(vemdog_store).
-behaviour(gen_server).

-export([init/1, reset/0, start_link/0, handle_call/3, handle_cast/2, handle_info/2]).
-export([to_json/0, summarize/0]).
%% TODO: -behaviour(erl_tracer).

-define(EV_STORE_TAB, vemdog_trace_events).
-define(UNIQ_PID_TAB, vemdog_unique_pids).

%% Trace using monotonic timestamp makes it unique and good to be a key
-record(ev, {
    %% Unique timestamp suitable for being a key
    timestamp :: binary(),
    type :: atom(),
    pid :: pid() | port(),
    args = [] :: list()
}).

%% gen_server state
-record(vemdog_store, {
    %% ETS table which contains flow of events, keyed by unique timestamp
    ev_store :: ets:tid(),
    %% ETS table which contains summarized pids, one row per unique pid
    unique_pids :: ets:tid(),
    scheduling_events = false :: boolean()
}).

to_json() ->
    gen_server:call(?MODULE, to_json).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    % ordered_set ?
    EvStore = ets:new(?EV_STORE_TAB, [named_table, public, {keypos, #ev.timestamp}]),
    UniquePids = ets:new(?UNIQ_PID_TAB, [named_table, public, {keypos, 1}]),
    {ok, #vemdog_store{
        ev_store = EvStore,
        unique_pids = UniquePids
    }}.

reset() ->
    ets:delete_all_objects(?EV_STORE_TAB).

summarize() ->
    gen_server:call(?MODULE, summarize, infinity).

prepare_row_to_json(#ev{timestamp = <<Ts:64/unsigned>>, pid = P, type = Ty, args = A}) ->
    #{ts => bin_ts_to_str(Ts), p => P, ty => Ty, a => A}.

handle_call(summarize, _From, State) ->
    {reply, summarize_(State), State};
handle_call(to_json, _From, State) ->
    % SortFn = fun(JsonRow1, JsonRow2) -> end,
    Rows = lists:map(fun prepare_row_to_json/1, ets:tab2list(?EV_STORE_TAB)),
    J = vemdog_jsone:encode(Rows, []),
    {reply, J, State};
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(Ev, State = #vemdog_store{scheduling_events = ShowScheduling}) when
    element(1, Ev) =:= 'trace_ts'
->
    %% Timestamp is always last element of the tuple
    case trace_event_to_ev(Ev, ShowScheduling) of
        false -> ok;
        Row -> ets:insert(?EV_STORE_TAB, Row)
    end,
    {noreply, State}.

%%to_str(T) -> T.
to_str(Term) -> iolist_to_binary(io_lib:format("~999999p", [Term])).

%%mfa_to_str(T) -> T.
mfa_to_str(0) -> <<"-">>;
mfa_to_str({M, F, Arity}) -> iolist_to_binary(io_lib:format("~s:~s/~p", [M, F, Arity])).

%% Format timestamp as long hex string
%%ts_to_str({T1, T2}) -> <<<<Y>> || <<X:4>> <= <<T1:64, T2:64>>, Y <- integer_to_list(X, 16)>>.

%% Format binary compact timestamp as hex for JSON export
bin_ts_to_str(Bin) -> <<<<(integer_to_list(X, 16))>> || <<X:4>> <= Bin>>.

%% Turn integer timestamp into a binary for compactness
ts_to_bin(T1) -> <<T1:64/unsigned>>.

trace_event_to_ev({trace_ts, PidPort, Send, Msg, To, Timestamp}, _ShowScheduling) when
    Send =:= send; Send =:= send_to_non_existing_process
->
    #ev{
        timestamp = ts_to_bin(Timestamp),
        type = Send,
        pid = to_str(PidPort),
        args = [to_str(Msg), To]
    };
trace_event_to_ev({trace_ts, PidPort, 'receive', Msg, Timestamp}, _ShowScheduling) ->
    %% TODO: Convert Msg to string/binary and trim length? Optionally?
    #ev{
        timestamp = ts_to_bin(Timestamp),
        type = 'receive',
        pid = to_str(PidPort),
        args = [to_str(Msg)]
    };
trace_event_to_ev({trace_ts, _PidPort, Sched, _MFA, _Timestamp}, _ShowScheduling = false) when
    Sched =:= in; Sched =:= in_exiting; Sched =:= out; Sched =:= out_exiting; Sched =:= out_exited
->
    false;
trace_event_to_ev({trace_ts, PidPort, Sched, MFA, Timestamp}, _ShowScheduling = true) when
    Sched =:= in; Sched =:= in_exiting; Sched =:= out; Sched =:= out_exiting; Sched =:= out_exited
->
    #ev{
        timestamp = ts_to_bin(Timestamp),
        type = Sched,
        pid = to_str(PidPort),
        args = [mfa_to_str(MFA)]
    };
trace_event_to_ev({trace_ts, PidPort, exit, Reason, Timestamp}, _ShowScheduling) ->
    #ev{
        timestamp = ts_to_bin(Timestamp),
        type = exit,
        pid = to_str(PidPort),
        args = [to_str(Reason)]
    };
trace_event_to_ev({trace_ts, PidPort, Spawn, Pid2, MFA, Timestamp}, _ShowScheduling) when
    Spawn =:= spawn; Spawn =:= spawned
->
    #ev{
        timestamp = ts_to_bin(Timestamp),
        type = Spawn,
        pid = to_str(PidPort),
        args = [to_str(Pid2), mfa_to_str(MFA)]
    };
trace_event_to_ev({trace_ts, PidPort, Link, Pid2, Timestamp}, _ShowScheduling) when
    Link =:= link; Link =:= unlink; Link =:= getting_linked; Link =:= getting_unlinked
->
    #ev{
        timestamp = ts_to_bin(Timestamp), type = Link, pid = to_str(PidPort), args = [to_str(Pid2)]
    };
trace_event_to_ev({trace_ts, PidPort, GC, _Params, Timestamp}, _ShowScheduling) when
    GC =:= gc_minor_start; GC =:= gc_minor_end; GC =:= gc_major_start; GC =:= gc_major_end
->
    #ev{timestamp = ts_to_bin(Timestamp), type = GC, pid = to_str(PidPort), args = []};
trace_event_to_ev({trace_ts, PidPort, Register, Name, Timestamp}, _ShowScheduling) when
    Register =:= register; Register =:= unregister
->
    #ev{
        timestamp = ts_to_bin(Timestamp),
        type = Register,
        pid = to_str(PidPort),
        args = [to_str(Name)]
    };
trace_event_to_ev(Other, _ShowScheduling) ->
    io:format(standard_error, "Unrecognized pattern in call ev_to_record(~p)~n", [Other]),
    #ev{timestamp = element(tuple_size(Other), Other), type = failed, args = Other}.

%% Run through the events and fill the table of unique pids
summarize_(#vemdog_store{}) ->
    ets:delete_all_objects(?UNIQ_PID_TAB),
    ets:foldl(
        fun(#ev{pid = P}, Acc) ->
            ets:insert_new(?UNIQ_PID_TAB, {P}),
            Acc
        end,
        [],
        ?EV_STORE_TAB
    ).
