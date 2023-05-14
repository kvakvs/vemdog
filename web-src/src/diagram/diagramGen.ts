import {StringSet, TraceEv} from "./TraceGrid";

const TRIM_LENGTH = 50;

const dquote = (s: string): string => {
  const trimmed = s.substring(0, TRIM_LENGTH) + (s.length > TRIM_LENGTH ? "…" : "");
  return trimmed.replace("\"", "\\\"");
}

export const buildFromEvents = (data: TraceEv[], shownPids: StringSet) => {
  let events: string[] = [];

  const evSend = (ev: TraceEv) => {
    // send args[0] is message, args[1] is destination pid
    const [msg, toPid] = ev.args;
    if (shownPids.has(toPid)) {
      events.push(`${ev.pid} -> ${toPid} : "send\n${dquote(msg)}"`)
    } else {
      events.push(`${ev.pid} -> * : "send to ${toPid}\n${dquote(msg)}"`)
    }
  }
  const evReceive = (ev: TraceEv) => {
    // receive does not show the sender
    const msg = ev.args[0];
    events.push(`* --> ${ev.pid} : "receive\n${dquote(msg)}"`)
  }
  const evSendNex = (ev: TraceEv) => {
    // send args[0] is message, args[1] is destination pid
    const toPid = ev.args[1];
    if (shownPids.has(toPid)) {
      events.push(`${ev.pid} -x ${toPid} : send`)
    } else {
      events.push(`${ev.pid} -x * : send to ${toPid}`)
    }
  }

  const evLink = (ev: TraceEv) => {
    // link args[0] is pid
    const toPid = ev.args[1];
    if (shownPids.has(toPid)) {
      events.push(`${ev.pid} --> ${toPid} : link`)
    } else {
      events.push(`${ev.pid} --> * : link to ${toPid}`)
    }
  }
  const evIsLinked = (ev: TraceEv) => {
    // link args[0] is pid
    const byPid = ev.args[1]
    if (shownPids.has(byPid)) {
      events.push(`${ev.pid} <-- ${byPid} : is linked`)
    } else {
      events.push(`${ev.pid} <-- * : is linked by ${byPid}`)
    }
  }
  const evSpawn = (ev: TraceEv) => {
    // link args[0] is pid spawned
    const newPid = ev.args[0]
    if (shownPids.has(newPid)) {
      events.push(`${ev.pid} -> *${newPid} : spawn`)
    } else {
      events.push(`${ev.pid} -> * : spawned ${newPid}`)
    }
  }
  const evExit = (ev: TraceEv) => {
    events.push(`note over ${ev.pid} : "exit with\n${ev.args[0]}"`)
    events.push(`end ${ev.pid}`)
  }

  const evGc = (ev: TraceEv) => {
    events.push(`note over ${ev.pid}: ${ev.type}`)
  }

  data.forEach((ev) => {
    if (!shownPids.has(ev.pid)) {
      return events;
    }

    if (ev.type === "send") {
      evSend(ev);
    } else if (ev.type === "send_to_non_existing_process") {
      evSendNex(ev)
    } else if (ev.type === "receive") {
      evReceive(ev)
    } else if (ev.type === "link") {
      evLink(ev)
    } else if (ev.type === "getting_unlinked" || ev.type === "spawned") {
      // nothing
    } else if (ev.type === "getting_linked") {
      evIsLinked(ev)
    } else if (ev.type === "spawn") {
      evSpawn(ev)
    } else if (ev.type === "exit") {
      evExit(ev)
    } else if (ev.type === "register") {
      events.push(`note over ${ev.pid}: register ${ev.args[0]}`)
    } else if (ev.type === "unregister") {
      events.push(`note over ${ev.pid}: unregister ${ev.args[0]}`)
    } else if (ev.type.startsWith("gc_")) {
      evGc(ev)
    } else {
      console.log("unhandled event", ev);
    }
  });
  events.push("terminators bar")
  return events.join("\n");
}
