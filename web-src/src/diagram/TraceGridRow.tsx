import {FC} from "react";
import {TraceDict, TraceEv, StringSet} from "./TraceGrid";

interface TraceGridRowProps {
  groupedByPid: TraceDict;
  shownPids: string[];
  rowIndex: number;
}

const createTooltip = (args: string[]) => {
  return args.join("\n");
}

interface TraceEventProps {
  row: TraceEv;
  colIndex: number;
}

const TraceEvent: FC<TraceEventProps> = ({row, colIndex}) => {
  // if (row.type === "send")
  return <div className="item" title={createTooltip(row.args)} key={`col${row.timestamp}-${colIndex}`}>{row.type}</div>;
}

export const TraceGridRow: FC<TraceGridRowProps> = ({groupedByPid, shownPids, rowIndex}) => {
  return (<div className="grid" key={`row${rowIndex}`}>
    <div className="item">-</div>
    {
      Array.from(shownPids).map((pid, i) => {
        const row = groupedByPid[pid][rowIndex];
        return row ? <TraceEvent row={row} colIndex={i} /> :
            <div className="noItem" key={`none${i}`}/>;
      })
    }</div>);
}
