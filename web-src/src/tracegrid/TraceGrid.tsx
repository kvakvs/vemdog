import {FC, useState} from "react";
import "./grid.scss";

interface TraceEv {
  timestamp: number;
  pid: string;
}

interface TraceGridProps {
  data: TraceEv[]
}

type TraceObjectsSet = Set<string>

const getUniquePids = (data: TraceEv[]): TraceObjectsSet => {
  return data.reduce((accum, d) => {
    accum.add(d.pid);
    return accum;
  }, new Set<string>());
}

export const TraceGrid: FC<TraceGridProps> = ({data}) => {
  const [shownPids, setShownPids] = useState<TraceObjectsSet>(new Set());
  const [hiddenPids, setHiddenPids] = useState<TraceObjectsSet>(getUniquePids(data));

  // <div className="grid">
  //   <div className="item">1</div>
  //   <div className="item">2</div>
  //   <div className="item">3</div>
  //   <div className="item">4</div>
  //   <div className="item">5</div>
  //   <div className="item">6</div>
  //   <div className="item">7</div>
  //   <div className="item">8</div>
  //   <div className="item">9</div>
  // </div>
  return (<div className="grid">
    {Array.from(shownPids).map((pid) => (<div className="item">{pid}</div>))}
    {Array.from(hiddenPids).map((pid) => (<div className="item hidden">{pid}</div>))}
  </div>);
}