%%% @author Dmytro Lytovchenko <dmytro.lytovchenko@gmail.com>
%%% @doc Application startup module for Vemdog trace explorer
-module(vemdog_app).

-behaviour(application).

-export([start/2, stop/1, start_web/0, print_web_server_info/0]).

%% Called automatically by the application library
start(_StartType, _StartArgs) ->
    vemdog_sup:start_link().

stop(_State) ->
    ok.

%% @doc Call this after the application started
start_web() ->
    inets:start(),
    {ok, ConfiguredPort} = application:get_env(vemdog, port),
    {ok, ConfiguredHost} = application:get_env(vemdog, host),
    {ok, ParsedHost} = inet_parse:address(ConfiguredHost),
    DocRoot = priv_dir(tx),
    SrvRoot = filename:absname(DocRoot ++ "/../"),

    Options = [
        {modules, [mod_esi, mod_get]},
        {port, ConfiguredPort},
        {server_name, "Vemdog"},
        {server_root, SrvRoot},
        {document_root, DocRoot},
        {bind_address, ParsedHost},

        %% Backend calls from the webpage go here
        {erl_script_alias, {"/vemdog", [vemdog_esi, io]}},

        {mime_types, [
            {"html", "text/html"}, {"css", "text/css"}, {"js", "application/x-javascript"}
        ]},
        {mime_type, "application/octet-stream"}
    ],
    {ok, Pid} = inets:start(httpd, Options),
    case ConfiguredPort of
        0 ->
            Info = httpd:info(Pid),
            {port, ListenPort} = proplists:lookup(port, Info),
            application:set_env(tx, port, ListenPort);
        _ ->
            ok
    end,

    io:format(
        "~n[vemdog] http server started. Start trace using vemdog:start() or vemdog:start(Spec, Options), or~n"
    ),
    print_web_server_info().

print_web_server_info() ->
    {ok, SelectedPort} = application:get_env(vemdog, port),
    {ok, ConfiguredHost} = application:get_env(vemdog, host),
    io:format("[vemdog] Visit http://~s:~p/index.html~n", [ConfiguredHost, SelectedPort]).

priv_dir(App) ->
    case code:priv_dir(App) of
        {error, bad_name} ->
            {ok, Cwd} = file:get_cwd(),
            Cwd ++ "/" ++ "priv/";
        Priv ->
            Priv ++ "/"
    end.
