# Manual Testing

Copy & Paste the following snippets to try and establish your own workflow:

1. `rebar3 shell`
   1. For Windows: `escript rebar3 shell` assuming there's rebar3 in current directory.
2. Start App, tracing, call tracing all in one command (auto stop after 10 sec):
   `vemdog:start(), dbg:tpl(erlang, []), vemdog:t(), observer:start().`
