import {FC} from "react";
import {TraceDict, TraceObjectsSet} from "./TraceGrid";

interface TraceGridRowProps {
  groupedByPid: TraceDict;
  shownPids: TraceObjectsSet;
  rowIndex: number;
}

export const TraceGridRow: FC<TraceGridRowProps> = ({groupedByPid, shownPids, rowIndex}) => {
  return (<div className="grid">
    <div className="item">-</div>
    {
      Array.from(shownPids).map((pid) => {
        const row = groupedByPid[pid][rowIndex];
        return (<div className="item">{row ? row.type : ""}</div>);
      })
  }</div>);
}
