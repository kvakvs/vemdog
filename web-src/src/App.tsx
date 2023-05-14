import React, {useEffect} from 'react';
import './vemdog.scss';
import {TraceEv, TraceGrid} from "./tracegrid/TraceGrid";
import data from './out.json';

const fromBackend = (parsed: any): TraceEv[] => {
  if (Array.isArray(parsed)) {
    return parsed.map((row): TraceEv => {
      return {timestamp: row.t, pid: row.p, type: row.ty, args: row.a}
    });
  }
  return [];
}

function App() {
  useEffect(() => {
    document.title = 'Vem dog? (who died?) Erlang trace explorer';
  }, []);
  const data1 = fromBackend(data);
  console.log(data1);
  return (
      <div className="App">
        <header className="header">
          <small>Vem dog? (Who died?) Erlang trace data explorer.</small>
        </header>
        <TraceGrid data={data1}/>
      </div>
  );
}

export default App;
