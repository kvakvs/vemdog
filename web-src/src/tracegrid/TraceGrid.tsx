import React, {FC, useMemo, useReducer, useState} from "react";
import "./grid.scss";
import {TraceGridRow} from "./TraceGridRow";
import {TraceGridTopRow} from "./TraceGridTopRow";

export interface TraceEv {
  timestamp: string;
  pid: string;
  type: string;
  args: string[];
}

interface TraceGridProps {
  data: TraceEv[]
}

export type StringSet = Set<string>
export type TraceEvOrNull = TraceEv | null;
export type TraceDict = { [key: string]: TraceEvOrNull[] }

const getUniquePids = (data: TraceEv[]): StringSet => {
  return data.reduce((accum, d) => {
    accum.add(d.pid);
    return accum;
  }, new Set<string>());
}

export const TraceGrid: FC<TraceGridProps> = ({data}) => {
  const [shownPids, setShownPids] = useState<StringSet>(new Set);
  const [hiddenPids, setHiddenPids] = useState<StringSet>(getUniquePids(data));

  // Shown and hidden sets of pid are combined into all pids
  // const allPids = useMemo<string[]>(() => {
  //   const s = Array.from(shownPids);
  //   const h = Array.from(hiddenPids);
  //   return s.concat(h);
  // }, [shownPids, hiddenPids]);

  const [ignored, forceUpdate] = useReducer(x => x + 1, 0);

  const shownPidsSorted = useMemo(() => Array.from(shownPids).sort(), [shownPids]);
  const hiddenPidsSorted = useMemo(() => Array.from(hiddenPids).sort(), [hiddenPids]);

  // Long input data list is split by pids into lists of events per pid
  const groupedByPid = useMemo<TraceDict>(() => {
    // Initialize all cells for each existing pid with empty lists
    const initial = shownPidsSorted.reduce(
        (accum, p) => {
          accum[p] = [];
          return accum;
        }, {} as TraceDict
    )

    // Use first N digits of timestamp to compare time; second 16 digits are unique integer
    const TIMESTAMP_COMPARE_N = 15;
    let prevTs = "-".repeat(TIMESTAMP_COMPARE_N);
    // Save previous pid, to group following rows if their timestamp is close enough
    let prevPid = "";

    // Sort events one per pid, and fill other pids in each row with empties
    return data.reduce((accum, ev) => {
      if (!shownPids.has(ev.pid)) {
        return accum; // do nothing
      }

      // Trim first N digits of the timestamp (16 full timestamp) and compare with previous row's timestamp
      const evTsTrim = ev.timestamp.substring(0, TIMESTAMP_COMPARE_N)

      if (evTsTrim == prevTs && prevPid !== ev.pid) {
        // Group with previous row, only if there was a null event for this current pid
        const prevRow = accum[ev.pid];
        prevRow[prevRow.length - 1] = ev;
        console.log(`grouping pid ${ev.pid} and previous ${prevPid}`)
      } else {
        // Not group together
        accum[ev.pid].push(ev);
        // update all other pids with empties
        shownPidsSorted.forEach((p) => {
          if (p !== ev.pid) {
            accum[p].push(null);
          }
        })

        prevTs = evTsTrim;
        prevPid = ev.pid;
      }
      return accum;
    }, initial);
  }, [data, shownPidsSorted]);

  const showPid = (pid: string) => {
    setShownPids((s) => {
      return s.add(pid);
    })
    setHiddenPids((h) => {
      h.delete(pid);
      return h;
    })
    forceUpdate()
  }

  const hidePid = (pid: string) => {
    setHiddenPids((h) => {
      return h.add(pid);
    })
    setShownPids((s) => {
      s.delete(pid);
      return s;
    })
    forceUpdate()
  }

  return (<>
    <TraceGridTopRow hidePid={hidePid} showPid={showPid} shownPids={shownPidsSorted} hiddenPids={hiddenPidsSorted}/>
    {data.map((_, i) => <TraceGridRow groupedByPid={groupedByPid} shownPids={shownPidsSorted} rowIndex={i}/>)}
  </>);
}