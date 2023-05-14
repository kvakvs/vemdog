# Vem dog (Who Died?)

1. Enables BEAM virtual machine tracing and collects the stream of events till the process
   you watch dies, or timeout occurs. The trace expands to newly spawned, linked and monitored processes helping you see
   the full picture.
2. Stores recorded events in memory, and feeds them into frontend also served by `vemdog`.
3. The frontend shows the interactive diagram with life of all recorded process and what happened to them.
4. Has no external dependencies, single self-contained Erlang application to be dropped into your source tree
   for investigation.

## Using

* `vemdog:start()` to start the app, including web server and a simple storage module which will receive the events.

* `vemdog:t/0` will reset the tracing data and begin tracing all new processes spawned after this call. If
  something links or is linked to, it will also be included in the trace. Also traces all calls to/from, scheduling in
  and out, and garbage collect events.

* Functions `vemdog:t/1` and `vemdog:t/2` take `Options` parameter which is a list with optional
  atoms: `gc` to include minor and major gc events on processes, and `scheduler` to include scheduling in, out, exiting
  events for processes and ports.

* `vemdog:stop()` will immediately stop tracing but all collected events will wait in the ETS storage to be
  visualized. Go to your web browser, and open localhost:1999 (or whatever port you configured in the vemdog application
  env).

* Go to `http://localhost:1999/index.html` (vemdog's webserver doesn't have concept of directory index, like other web
  servers, so you have to type `index.html` explicitly).

## Features Done and Missing

[See issue #1](https://github.com/kvakvs/vemdog/issues/1)