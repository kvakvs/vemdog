# Manual Testing

Copy & Paste the following snippets to try and establish your own workflow:

1. `rebar3 shell`
   1. For Windows: `escript rebar3 shell` assuming there's rebar3 in current directory.
 
3. Start App, tracing, call tracing all in one command (auto stop after 10 sec):
   `vemdog:start(), vemdog:t(), observer:start().`
   1. Same with GC:
      `vemdog:start(), vemdog:t(new, [gc]), observer:start().`

3. Dump JSON file for investigation: `file:write_file("out.json", vemdog_store:to_json()).`