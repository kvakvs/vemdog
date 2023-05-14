import React, {FC, useMemo} from "react";
import "./grid.scss";
import {TraceGridTopRow} from "./TraceGridTopRow";

export interface TraceEv {
  // timestamp: string; // not used in the diagram
  pid: string;
  type: string;
  args: string[];
}

export type StringSet = Set<string>
// export type TraceEvOrNull = TraceEv | null;
// export type TraceDict = { [key: string]: TraceEvOrNull[] }

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
  );
}