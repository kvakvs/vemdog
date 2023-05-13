import React, {useEffect} from 'react';
import './vemdog.scss';
import {TraceGrid} from "./tracegrid/TraceGrid";

function App() {
  useEffect(() => {
    document.title = 'Vem dog? (who died?) Erlang trace explorer';
  }, []);
  const testData = [
    {timestamp: 0, pid: "<0.0.0>"},
    {timestamp: 1, pid: "<0.1.0>"},
    {timestamp: 2, pid: "<0.2.0>"},
  ];
  return (
      <div className="App">
        <header className="header">
          <small>Vem dog? (Who died?) Erlang trace data explorer.</small>
        </header>
        <TraceGrid data={testData} />
      </div>
  );
}

export default App;
