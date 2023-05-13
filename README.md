# Vem dog (Who Died?)

1. Enables full system tracing and collects the stream of events till the process
   you watch dies, or timeout occurs.
2. Stores recorded events in a searchable index
3. Shows the life of the process and what happened to it using a web page and
   message sequence chart (SVG).

## Using

* `vemdog:start()` to start the app, including web server and a simple storage module which will receive the events.

* `vemdog:trace/0` will reset the tracing data and begin tracing all new processes spawned after this call. If
  something links or is linked to, it will also be included in the trace. Also traces all calls to/from, scheduling in
  and out, and garbage collect events.

* Functions `vemdog:trace/1` and `trace/2` take `Options` parameter which is a list with optional
  atoms `gc` to include minor and major gc events on processes, and `scheduler` to include scheduling in, out, exiting
  events for processes and ports.

* `vemdog:stop_trace()` will immediately stop tracing but all collected events will wait in the ETS storage to be
  visualized. Go to your web browser, and open localhost:1999 (or whatever port you configured in the vemdog application
  env).