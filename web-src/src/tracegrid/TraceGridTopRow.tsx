import {FC} from "react";
import {TraceObjectsSet} from "./TraceGrid";
import "./grid.scss";

interface TraceGridTopRowProps {
  showPid: (p: string) => void;
  hidePid: (p: string) => void;
  shownPids: TraceObjectsSet;
  hiddenPids: TraceObjectsSet;
}


export const TraceGridTopRow: FC<TraceGridTopRowProps> = ({shownPids, hiddenPids, showPid, hidePid}) => {
  const shownHeaderCell = (pid: string) => {
    return <div className="item rowHeader">{pid}
      <button className="gridUi" onClick={() => hidePid(pid)}>-</button>
    </div>;
  }

  const hiddenHeaderCell = (pid: string) => {
    return <div className="item rowHeader hidden">{pid}
      <button className="gridUi" onClick={() => showPid(pid)}>+</button>
    </div>;
  }

  return <div className="grid">
    <div className="item">↓ Timeline</div>
    {Array.from(shownPids).map(shownHeaderCell)}
    {Array.from(hiddenPids).map(hiddenHeaderCell)}
  </div>;
}
