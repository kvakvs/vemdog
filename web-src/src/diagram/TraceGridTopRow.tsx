import {FC} from "react";
import "./grid.scss";

interface TraceGridTopRowProps {
  showPid: (p: string) => void;
  hidePid: (p: string) => void;
  shownPids: string[];
  hiddenPids: string[];
}


const trimPid = (pid: string): string => {
  return pid.replace(/^<|>$/g, '');
}

export const TraceGridTopRow: FC<TraceGridTopRowProps> = ({shownPids, hiddenPids, showPid, hidePid}) => {
  const shownHeaderCell = (pid: string, i: number) => {
    return <div className="item rowHeader" key={`header${i}`}>
      <button className="uiHide" onClick={() => hidePid(pid)}>{trimPid(pid)}</button>
    </div>;
  }

  const hiddenHeaderCell = (pid: string, i: number) => {
    return <div className="item rowHeader hidden" key={`hiddenHeader${i}`}>
      <button className="uiShow" onClick={() => showPid(pid)}>{trimPid(pid)}</button>
    </div>;
  }

  return <div className="grid">
    {/*<div className="item">↓ Timeline</div>*/}
    {Array.from(shownPids).map(shownHeaderCell)}
    {Array.from(hiddenPids).map(hiddenHeaderCell)}
  </div>;
}
