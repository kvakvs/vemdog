import React, {FC, useMemo} from "react";
import "./grid.scss";
import {TraceGridTopRow} from "./TraceGridTopRow";

export interface TraceEv {
  timestamp: string;
  pid: string;
  type: string;
  args: string[];
}

export type StringSet = Set<string>
export type TraceEvOrNull = TraceEv | null;
export type TraceDict = { [key: string]: TraceEvOrNull[] }

interface TraceGridProps {
  // data: TraceEv[];
  shownPids: StringSet;
  hiddenPids: StringSet;
  setShownPids: (fn: (old: StringSet) => StringSet) => void;
  setHiddenPids: (fn: (old: StringSet) => StringSet) => void;
}


export const TraceGrid: FC<TraceGridProps> = ({
                                                // data,
                                                shownPids,
                                                setShownPids,
                                                hiddenPids,
                                                setHiddenPids,
                                              }) => {
  const shownPidsSorted = useMemo(() => Array.from(shownPids).sort(), [shownPids]);
  const hiddenPidsSorted = useMemo(() => Array.from(hiddenPids).sort(), [hiddenPids]);

  // Long input data list is split by pids into lists of events per pid
  // const groupedByPid = useMemo<TraceDict>(() => {
  //   // Initialize all cells for each existing pid with empty lists
  //   const initial = shownPidsSorted.reduce(
  //       (accum, p) => {
  //         accum[p] = [];
  //         return accum;
  //       }, {} as TraceDict
  //   )
  //
  //   // Use first N digits of timestamp to compare time; second 16 digits are unique integer
  //   const TIMESTAMP_COMPARE_N = 15;
  //   let prevTs = "-".repeat(TIMESTAMP_COMPARE_N);
  //   // Save previous pid, to group following rows if their timestamp is close enough
  //   let prevPid = "";
  //
  //   // Sort events one per pid, and fill other pids in each row with empties
  //   return data.reduce((accum, ev) => {
  //     if (!shownPids.has(ev.pid)) {
  //       return accum; // do nothing
  //     }
  //
  //     // Trim first N digits of the timestamp (16 full timestamp) and compare with previous row's timestamp
  //     const evTsTrim = ev.timestamp.substring(0, TIMESTAMP_COMPARE_N)
  //
  //     if (evTsTrim == prevTs && prevPid !== ev.pid) {
  //       // Group with previous row, only if there was a null event for this current pid
  //       const prevRow = accum[ev.pid];
  //       prevRow[prevRow.length - 1] = ev;
  //       console.log(`grouping pid ${ev.pid} and previous ${prevPid}`)
  //     } else {
  //       // Not group together
  //       accum[ev.pid].push(ev);
  //       // update all other pids with empties
  //       shownPidsSorted.forEach((p) => {
  //         if (p !== ev.pid) {
  //           accum[p].push(null);
  //         }
  //       })
  //
  //       prevTs = evTsTrim;
  //       prevPid = ev.pid;
  //     }
  //     return accum;
  //   }, initial);
  // }, [data, shownPidsSorted]);

  const showPid = (pid: string) => {
    setShownPids((s) => {
      const s1 = s.add(pid);
      return new Set(s1);
    })
    setHiddenPids((h) => {
      h.delete(pid);
      return new Set(h);
    })
  }

  const hidePid = (pid: string) => {
    setHiddenPids((h) => {
      const h1 = h.add(pid);
      return new Set(h1);
    })
    setShownPids((s) => {
      s.delete(pid);
      return new Set(s);
    })
  }

  return (
      <TraceGridTopRow hidePid={hidePid} showPid={showPid} shownPids={shownPidsSorted} hiddenPids={hiddenPidsSorted}/>
      // {data.map((_, i) => <TraceGridRow groupedByPid={groupedByPid} shownPids={shownPidsSorted} rowIndex={i}/>)}
  );
}