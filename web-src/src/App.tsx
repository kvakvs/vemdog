import React, {useEffect} from 'react';
import './vemdog.scss';
import {TraceEv, TraceGrid} from "./tracegrid/TraceGrid";

const fromBackend = (s: string): TraceEv[] => {
  const parsed = JSON.parse(s);
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
  const testData = fromBackend('[{"args":["{#Ref<0.2363805254.3062366214.213332>,{ok,{once,#Ref<0.2363805254.3062366214.213333>}}}","<0.135.0>"],"pid":"<0.148.0>","timestamp":"0000000247A28E58F800000000000008","type":"send"},{"args":["{ack,<0.148.0>,{ok,<0.148.0>}}","<0.49.0>"],"pid":"<0.148.0>","timestamp":"0000000247A0FE58F800000000000004","type":"send"},{"args":["<0.150.0>","vemdog:stop\\/[]"],"pid":"<0.148.0>","timestamp":"000000049C0BCE58F800000000000013","type":"spawn"},{"args":["<0.49.0>","proc_lib:init_p\\/[kernel_sup,\\n                 [<0.47.0>],\\n                 gen,init_it,\\n                 [gen_server,<0.49.0>,<0.49.0>,\\n                  {local,timer_server},\\n                  timer,[],[]]]"],"pid":"<0.148.0>","timestamp":"0000000247A0FE58F800000000000000","type":"spawned"},{"args":["<0.134.0>"],"pid":"<0.149.0>","timestamp":"0000000247A41E58F80000000000000D","type":"getting_linked"},{"args":["<0.134.0>","erlang:apply\\/[#Fun<shell.1.97416695>,[]]"],"pid":"<0.149.0>","timestamp":"0000000247A41E58F80000000000000C","type":"spawned"},{"args":["<0.148.0>","vemdog:stop\\/[]"],"pid":"<0.150.0>","timestamp":"000000049C0BCE58F800000000000014","type":"spawned"},{"args":["<0.49.0>"],"pid":"<0.148.0>","timestamp":"0000000247A0FE58F800000000000001","type":"getting_linked"},{"args":["{timeout,#Ref<0.2363805254.3062366214.213333>,{apply_once,{vemdog,stop,[]}}}"],"pid":"<0.148.0>","timestamp":"000000049C0BCE58F800000000000012","type":"receive"},{"args":["timer_server"],"pid":"<0.148.0>","timestamp":"0000000247A0FE58F800000000000003","type":"register"},{"args":["{io_request,<0.149.0>,#Ref<0.2363805254.3062366214.213339>,{get_until,unicode,[\\"2\\",62,32],erl_scan,tokens,[{1,1},[text,{reserved_word_fun,fun erl_scan:reserved_word\\/1}]]}}","<0.129.0>"],"pid":"<0.149.0>","timestamp":"0000000247A41E58F80000000000000F","type":"send"},{"args":["{\'$gen_call\',{<0.135.0>,#Ref<0.2363805254.3062366214.213332>},{apply_once,{9792,10000,{vemdog,stop,[]}}}}"],"pid":"<0.148.0>","timestamp":"0000000247A28E58F800000000000007","type":"receive"}]');
  console.log(testData);
  return (
      <div className="App">
        <header className="header">
          <small>Vem dog? (Who died?) Erlang trace data explorer.</small>
        </header>
        <TraceGrid data={testData}/>
      </div>
  );
}

export default App;
