import {FC, useMemo, useState} from "react";
import "./grid.scss";

export interface TraceEv {
  timestamp: string;
  pid: string;
  type: string;
  args: string[];
}

interface TraceGridProps {
  data: TraceEv[]
}

type TraceObjectsSet = Set<string>
type TraceDict = { [key: string]: TraceEv[] }

const getUniquePids = (data: TraceEv[]): TraceObjectsSet => {
  return data.reduce((accum, d) => {
    accum.add(d.pid);
    return accum;
  }, new Set<string>());
}

export const TraceGrid: FC<TraceGridProps> = ({data}) => {
  const [shownPids, setShownPids] = useState<TraceObjectsSet>(new Set());
  const [hiddenPids, setHiddenPids] = useState<TraceObjectsSet>(getUniquePids(data));

  // Shown and hidden sets of pid are combined into all pids
  const allPids = useMemo<string[]>(() => {
    const s = Array.from(shownPids);
    const h = Array.from(hiddenPids);
    return s.concat(h);
  }, [shownPids, hiddenPids]);

  // Long input data list is split by pids into lists of events per pid
  const groupedByPid = useMemo<TraceDict>(() => {
    return data.reduce((accum, ev) => {
      if (accum[ev.pid]) {
        accum[ev.pid].push(ev);
      } else {
        accum[ev.pid] = [ev];
      }
      return accum;
    }, {} as TraceDict);
  }, [data]);

  const headerCell = (pid: string) => {
    if (shownPids.has(pid)) {
      return <div className="item rowHeader">{pid}</div>;
    } else {
      return <div className="item rowHeader hidden">{pid}
        <button className="gridUi">Show</button>
      </div>;
    }
  }

  // Display top row of header cells
  const topRow = () => {
    return <div className="grid">
      <div className="item">↓ Timeline</div>
      {allPids.map(headerCell)}
    </div>;
  }

  // From a dictionary of data streams grouped by pid, pick only shown items
  // For all shown items try slice streams by timestamp
  const populateRows = () => {
    // Contains position in each pid's stream
    const lastIndex = Array.from(shownPids).map(() => 0);
    // Contains last timestamp in each pid's stream
    const lastTimestamp = Array.from(shownPids).map((p) => {
      // Since pid exists in allPids, it has at least one event so we can safely take first event
      return groupedByPid[p][0].timestamp;
    });
    return
  }

  return (<>
    {topRow()}
    {populateRows()}
  </>);
}