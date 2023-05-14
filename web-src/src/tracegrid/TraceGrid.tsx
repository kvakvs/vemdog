import React, {FC, useMemo, useState} from "react";
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

export type TraceObjectsSet = Set<string>
export type TraceEvOrNull = TraceEv | null;
export type TraceDict = { [key: string]: TraceEvOrNull[] }

const getUniquePids = (data: TraceEv[]): TraceObjectsSet => {
  return data.reduce((accum, d) => {
    accum.add(d.pid);
    return accum;
  }, new Set<string>());
}

export const TraceGrid: FC<TraceGridProps> = ({data}) => {
  const [shownPids, setShownPids] = useState<TraceObjectsSet>(getUniquePids(data));
  const [hiddenPids, setHiddenPids] = useState<TraceObjectsSet>(new Set());

  // Shown and hidden sets of pid are combined into all pids
  const allPids = useMemo<string[]>(() => {
    const s = Array.from(shownPids);
    const h = Array.from(hiddenPids);
    return s.concat(h);
  }, [shownPids, hiddenPids]);

  // Long input data list is split by pids into lists of events per pid
  const groupedByPid = useMemo<TraceDict>(() => {
    // Initialize all cells for each existing pid with empty lists
    const initial = allPids.reduce(
        (accum, p) => {
          accum[p] = [];
          return accum;
        }, {} as TraceDict
    )

    // Sort events one per pid, and fill other pids in each row with empties
    return data.reduce((accum, ev) => {
      accum[ev.pid].push(ev);
      // update all other pids with empties
      allPids.forEach((p) => {
        if (p !== ev.pid) {
          accum[p].push(null);
        }
      })
      return accum;
    }, initial);
  }, [data, allPids]);

  const showPid = (pid: string) => {
    setShownPids((s) => {
      s.add(pid);
      return s;
    })
    setHiddenPids((h) => {
      h.delete(pid);
      return h;
    })
    console.log("show", pid);
  }

  const hidePid = (pid: string) => {
    setHiddenPids((h) => {
      h.add(pid);
      return h;
    })
    setShownPids((s) => {
      s.delete(pid);
      return s;
    })
    console.log("hide", pid);
  }

  return (<>
    <TraceGridTopRow hidePid={hidePid} showPid={showPid} shownPids={shownPids} hiddenPids={hiddenPids}/>
    {data.map((_, i) => <TraceGridRow groupedByPid={groupedByPid} shownPids={shownPids} rowIndex={i}/>)}
  </>);
}