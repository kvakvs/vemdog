import {FC} from "react";
import "./grid.scss";

interface TraceGridTopRowProps {
  showPid: (p: string) => void;
  hidePid: (p: string) => void;
  shownPids: string[];
  hiddenPids: string[];
}


export const TraceGridTopRow: FC<TraceGridTopRowProps> = ({shownPids, hiddenPids, showPid, hidePid}) => {
  const shownHeaderCell = (pid: string, i: number) => {
    return <div className="item rowHeader" key={`header${i}`}>{pid}
      <button className="gridUi" onClick={() => hidePid(pid)}>-</button>
    </div>;
  }

  const hiddenHeaderCell = (pid: string, i: number) => {
    return <div className="item rowHeader hidden" key={`hiddenHeader${i}`}>{pid}
      <button className="gridUi" onClick={() => showPid(pid)}>+</button>
    </div>;
  }

  return <div className="grid">
    <div className="item">↓ Timeline</div>
    {Array.from(shownPids).map(shownHeaderCell)}
    {Array.from(hiddenPids).map(hiddenHeaderCell)}
  </div>;
}
