# Vem dog (Who Died?)

1. Enables BEAM virtual machine tracing and collects the stream of events till the process
   you watch dies, or timeout occurs. The trace expands to newly spawned, linked and monitored processes helping you see
   the full picture.
2. Stores recorded events in memory, and feeds them into frontend also served by `vemdog`.
3. The frontend shows the interactive diagram with life of all recorded process and what happened to them.
4. Has no external dependencies, single self-contained Erlang application to be dropped into your source tree
   for investigation.

## Early Prototype Pics

![Pic1](https://github.com/kvakvs/vemdog/assets/288724/51f01bdb-8084-4bfc-8c56-dfa7fb033800)

## Starting

* `vemdog:start()` to start the app, including web server and a simple storage module which will receive the events.

* `vemdog:t/0` will reset the tracing data and begin tracing all new processes spawned after this call. If
  something links or is linked to, it will also be included in the trace. Also traces all calls to/from, scheduling in
  and out, and garbage collect events.

* Functions `vemdog:t/1` and `vemdog:t/2` take `Options` parameter which is a list with optional
  atoms: `gc` to include minor and major gc events on processes, and `scheduler` to include scheduling in, out, exiting
  events for processes and ports.

* To trace function calls, also the trace pattern must be set. You can set trace pattern via `dbg:tp` (global function
  calls), `dbg:tpl` (local function calls). For example: `dbg:tpl(Module, [])`. See
  the [Erlang documentation for `dbg`](https://www.erlang.org/doc/man/dbg.html#tp-2).

## Stopping

* Vemdog will invoke `vemdog:stop()` after a 10 second timer.

* `vemdog:stop()` will immediately stop tracing but all collected events will wait in the ETS storage to be
  visualized.

## Browsing

* Go to `http://localhost:1999/index.html` (vemdog's webserver doesn't have concept of directory index, like other web
  servers, so you have to type `index.html` explicitly).

* You can grab the JSON instead: `file:write_file("out.json", vemdog_store:to_json()).` If the trace is very large, the
  JSON builder may consume nasty amounts of RAM and may take a while to run.

## Motivation on Low Performance

* High performance tracing requires a NIF implementation of a tracer. This is possible, but will make `vemdog` harder to
  build and to run from an existing project.
* High performance JSON libraries will run in a NIF and introduce an external dependency (right now `vemdog` contains 2
  ERL files which do the JSON encoding and no external dependencies.)
* Encoding ETS streams of events in smaller chunks: this is on the TODO list, certainly will help performance when
  exporting data to the frontend for diagram construction.

## Features Done and Missing

[See issue #1](https://github.com/kvakvs/vemdog/issues/1)