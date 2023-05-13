-module(vemdog_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

child(Ident, Type) ->
    #{
        id => Ident,
        start => {Ident, start_link, []},
        type => permanent,
        shutdown => 5000,
        type => Type,
        modules => [Ident]
    }.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok,
        {
            #{
                strategy => one_for_one,
                intensity => 5,
                period => 10
            },
            [child(vemdog_store, worker)]
        }}.
