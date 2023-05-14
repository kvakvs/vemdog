%%% @author Dmytro Lytovchenko <dmytro.lytovchenko@gmail.com>
%%% @doc User API, use from your test module, your program, or from Erlang shell
-module(vemdog).

-export([start/0, t/0, t/1, t/2, stop/0]).

-type pid_port_spec() ::
    pid()
    | port()
    | all
    | processes
    | ports
    | existing
    | existing_processes
    | existing_ports
    | new
    | new_processes
    | new_ports.
%% If present in options, will invoke erlang:trace_pattern instead of erlang:trace
%%-type trace_pattern() :: {module(), atom(), non_neg_integer() | '_'}.
-type vemdog_trace_opt() :: gc | scheduler.
% | {tp, trace_pattern()}.

%% Called manually by the user
start() ->
    case application:start(vemdog) of
        ok ->
            vemdog_app:start_web(),
            ok;
        {error, {already_started, _}} ->
            ok
    end.

%% Start tracing all new processes, and all processes spawned after that, and all newly linked processes too
t() ->
    %% trace/1 shadowed by erlang:trace/1
    ?MODULE:t(new).

%% @param Spec goes to erlang:trace directly, use new or new_processes or new_ports to trace only new, for example
-spec t(Spec :: pid_port_spec()) -> any().
t(Spec) ->
    %% trace/2 shadowed by erlang:trace/2
    ?MODULE:t(Spec, [scheduler]).

-spec t(Spec :: pid_port_spec(), Opts :: [vemdog_trace_opt()]) -> any().
t(Spec, Opts) ->
    stop_internal(),
    vemdog_store:reset(),
    trace_internal(Spec, true, Opts),
    StopTime = 10,
    timer:apply_after(timer:seconds(StopTime), ?MODULE, stop, []),
    io:format(
        "[vemdog] Starting trace for spec=~p; Stop after ~p sec or call vemdog:stop_trace().~n", [
            Spec, StopTime
        ]
    ).

-spec trace_internal(Spec :: pid_port_spec(), Enable :: boolean(), Opts :: [vemdog_trace_opt()]) -> any().
trace_internal(Spec, Enable, Opts) ->
    MaybeGc =
        case lists:member(gc, Opts) of
            %% Tags: gc_minor_start, gc_max_heap_size, and gc_minor_end.
            true ->
                [
                    garbage_collection
                ];
            false ->
                []
        end,
    MaybeScheduler =
        case lists:member(scheduler, Opts) of
            %% Tags: in, out
            true ->
                [
                    running,
                    running_procs,
                    %% Tags: in_exiting, out_exiting, and out_exited.
                    exiting
                ];
            false ->
                []
        end,
    FlagList =
        [
            %% Trace messages
            %% Tags: send and send_to_non_existing_process.
            send,
            %% Tags: 'receive'.
            'receive',
            %% Trace calls
            %% Tags: call and return_from.
            call,
            %% Reportno parameter or return values, only mfarity
            arity,
            %% Tags: return_to.
            return_to,
            %% Process-related: spawn, spawned, exit, register, unregister, link, unlink, getting_linked, and getting_unlinked.
            procs,
            %% Trace using monotonic timestamp makes it unique and good to be a key in trace store ETS table
            strict_monotonic_timestamp,
            %% Makes any process created by a traced process inherit its trace flags, including flag set_on_spawn.
            set_on_spawn,
            %% Makes any process linked by a traced process inherit its trace flags, including flag set_on_link.
            set_on_link,
            {tracer, whereis(vemdog_store)}
        ] ++ MaybeGc ++ MaybeScheduler,
    erlang:trace(Spec, Enable, FlagList).

%% Does not stop the application, only stops tracing. Prints.
stop() ->
    stop_internal(),
    io:format("~n[vemdog] Tracing stopped.~n"),
    vemdog_app:print_web_server_info().

%% Stop but no printing
stop_internal() ->
    %% Clear all tracing patterns. Stop all tracing with all options.
    dbg:ctp(),
    trace_internal(all, false, [gc, scheduler]).
