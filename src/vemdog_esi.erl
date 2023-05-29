%%% @author Dmytro Lytovchenko <dmytro.lytovchenko@gmail.com>
%%% @doc Used for the web frontend to call backend
-module(vemdog_esi).

%% API
-export([json/3]).

-type chunked_data() ::
    {first, Data :: binary()}
    | {continue, Data :: binary(), State :: term()}
    | {last, Data :: binary(), State :: term()}.

json(Sid, Env, In) -> try_do(fun do_json/3, Sid, Env, In).
do_json(Sid, _Env, _In) ->
    %% First chunk with headers MUST be a string
    Headers =
        "Content-Type: application/json\n"
        "\r\n\r\n",
    mod_esi:deliver(Sid, Headers),

    Resp = vemdog_store:to_json(),
    io:format(standard_error, "JSON reply: ~9999p~n", [Resp]),
    mod_esi:deliver(Sid, Resp).

%% @private
-spec try_do(
    F :: fun(), Sid :: pid(), Env :: proplists:proplist(), In :: string() | chunked_data()
) -> any().
try_do(F, Sid, Env, In) ->
    %% io:format(standard_error, "~9999p~n", [Env]),
    try
        F(Sid, Env, In)
    catch
        ErrType:Err:Stack ->
            Resp = io_lib:format("~p:~120p~n~120p", [ErrType, Err, Stack]),
            mod_esi:deliver(Sid, Resp)
    end.
