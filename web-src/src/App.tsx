import React, {useEffect, useState} from 'react';
import './vemdog.scss';
import {StringSet, TraceEv, TraceGrid} from "./diagram/TraceGrid";
import data from './out.json';
import {SVGDiagram} from "./diagram/SVGDiagram";

const fromBackend = (parsed: any): TraceEv[] => {
  if (Array.isArray(parsed)) {
    return parsed.map((row): TraceEv => {
      return {timestamp: row.t, pid: row.p, type: row.ty, args: row.a}
    });
  }
  return [];
}

const getUniquePids = (data: TraceEv[]): StringSet => {
  return data.reduce((accum, d) => {
    accum.add(d.pid);
    return accum;
  }, new Set<string>());
}

function App() {
  useEffect(() => {
    document.title = 'Vem dog? (who died?) Erlang trace explorer';
  }, []);
  const data1 = fromBackend(data);

  const [shownPids, setShownPids] = useState<StringSet>(new Set);
  const [hiddenPids, setHiddenPids] = useState<StringSet>(getUniquePids(data1));

  return (
      <div className="App">
        <header className="header">
          <small>Vem dog? (Who died?) Erlang trace data explorer.</small>
        </header>
        <TraceGrid {...{shownPids, setShownPids, hiddenPids, setHiddenPids}}/>
        <SVGDiagram {...{data: data1, shownPids}} />
      </div>
  );
}

export default App;
