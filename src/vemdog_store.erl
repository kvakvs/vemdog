%%% @author Dmytro Lytovchenko <dmytro.lytovchenko@gmail.com>
%%% @doc Stores a history of active trace in a public ETS table
-module(vemdog_store).

-export([init/1, reset/0, start_link/0, handle_call/3, handle_cast/2, handle_info/2]).
-behaviour(gen_server).
%% TODO: -behaviour(erl_tracer).

-define(ETS_TAB, vemdog_trace_events).

%% Trace using monotonic timestamp makes it unique and good to be a key
-record(ev, {
    %% Unique timestamp suitable for being a key
    timestamp,
    type :: atom(),
    pidport :: pid() | port(),
    args = [] :: list()
}).
%% gen_server state (empty)
-record(vemdog_store, {}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ets:new(?ETS_TAB, [named_table, public, {keypos, #ev.timestamp}]),
    {ok, #vemdog_store{}}.

reset() ->
    ets:delete_all_objects(?ETS_TAB).

handle_call(_Request, _From, State) -> {noreply, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(Ev, State) when element(1, Ev) =:= 'trace_ts' ->
    %% Timestamp is always last element of the tuple
    ets:insert(?ETS_TAB, ev_to_record(Ev)),
    {noreply, State}.

%%to_str(T) -> T.
to_str(Term) -> iolist_to_binary(io_lib:format("~999999p", [Term])).

%%mfa_to_str(T) -> T.
mfa_to_str(0) -> <<"-">>;
mfa_to_str({M, F, Arity}) -> iolist_to_binary(io_lib:format("~s:~s/~p", [M, F, Arity])).

ev_to_record({trace_ts, PidPort, Send, Msg, To, Timestamp}) when
    Send =:= send; Send =:= send_to_non_existing_process
->
    #ev{timestamp = Timestamp, type = Send, pidport = to_str(PidPort), args = [to_str(Msg), To]};
ev_to_record({trace_ts, PidPort, 'receive', Msg, Timestamp}) ->
    %% TODO: Convert Msg to string/binary and trim length? Optionally?
    #ev{timestamp = Timestamp, type = 'receive', pidport = to_str(PidPort), args = [to_str(Msg)]};
ev_to_record({trace_ts, PidPort, Sched, MFA, Timestamp}) when
    Sched =:= in; Sched =:= in_exiting; Sched =:= out; Sched =:= out_exiting; Sched =:= out_exited
->
    #ev{timestamp = Timestamp, type = Sched, pidport = to_str(PidPort), args = [mfa_to_str(MFA)]};
ev_to_record({trace_ts, PidPort, exit, Reason, Timestamp}) ->
    #ev{timestamp = Timestamp, type = exit, pidport = to_str(PidPort), args = [to_str(Reason)]};
ev_to_record({trace_ts, PidPort, Spawn, Pid2, MFA, Timestamp}) when
    Spawn =:= spawn; Spawn =:= spawned
->
    #ev{timestamp = Timestamp, type = Spawn, pidport = to_str(PidPort), args = [to_str(Pid2), mfa_to_str(MFA)]};
ev_to_record({trace_ts, PidPort, Link, Pid2, Timestamp}) when
    Link =:= link; Link =:= unlink; Link =:= getting_linked; Link =:= getting_unlinked
->
    #ev{timestamp = Timestamp, type = Link, pidport = to_str(PidPort), args = [to_str(Pid2)]};
ev_to_record({trace_ts, PidPort, GC, _Params, Timestamp}) when
    GC =:= gc_minor_start; GC =:= gc_minor_end; GC =:= gc_major_start; GC =:= gc_major_end
->
    #ev{timestamp = Timestamp, type = GC, pidport = to_str(PidPort), args = []};
ev_to_record({trace_ts, PidPort, register, Name, Timestamp}) ->
    #ev{timestamp = Timestamp, type = register, pidport = to_str(PidPort), args = [to_str(Name)]};
ev_to_record(Other) ->
    io:format(standard_error, "Unrecognized pattern in call ev_to_record(~p)~n", [Other]),
    #ev{timestamp = element(tuple_size(Other), Other), type = failed, args = Other}.
