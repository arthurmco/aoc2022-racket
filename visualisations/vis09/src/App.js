import './App.css';
import chroma from "chroma-js";
import { QueryClient, QueryClientProvider, useQuery } from 'react-query';

import { useState, useEffect } from 'react';

const queryClient = new QueryClient();

function App() {
    return (
        <QueryClientProvider client={queryClient}>
          <RopeView/>
        </QueryClientProvider>
    );
}

const removeDuplicates = (arr) => {
    return arr.filter(function(elem, pos) {
        return arr.findIndex(v => v[0] === elem[0] && v[1] === elem[1]) === pos;
    });
};

function RopeSnapshot({rope, offsetX, offsetY, tails}) {
    const head = rope.head;
    const tail = rope.tail;
    
    const absMiddle = removeDuplicates(rope.middle);

    const colorScale = chroma.scale(["#ff9999", "#99ff99", "#9999ff"]);

    const xFix = x => x-(offsetX/2);
    const yFix = y => 70+y-(offsetY/2);
    
    
    return (
        <div style={{border: "1px solid rgba(255, 255, 255, 0.1)"}}>
          <svg
	        width="1800px" height="1080px"
	        viewBox="0 10 130 200"
	        zoomAndPan="disable" >

            {
                tails.map(v => <rect key={v} x={xFix(v[0])} y={yFix(v[1])}
                                        width="1" height="1" fill="#888888" />)

            }

            <rect x={xFix(tail[0])} y={yFix(tail[1])} width="1" height="1" fill="#ff9999" />
            {
                absMiddle.reverse().map((v, index) =>
                    <rect key={v} x={xFix(v[0])} y={yFix(v[1])} width="1" height="1"
                          fill={colorScale((index+1)/(absMiddle.length+1))} />)
            }
            <rect x={xFix(head[0])} y={yFix(head[1])} width="1" height="1" fill="#9999ff" />

          </svg>
        </div>
    );

}

function RopeView() {
    const {isLoading, error, data} = useQuery('ropeKnots', () => {
        return fetch('http://localhost:8000/test.json').then(res => res.json());
    });

    const [index, setIndex] = useState(0);
    const [active, setActive] = useState(false);

    const toggleActive = () => {
        setActive(!active);
        setIndex(0);
    };
    
    useEffect(
        () => {
            if (!data) return;
            if (!active) return;
            
            const interval = setTimeout(_ => {
                setIndex(Math.min(index+1, data.length-1));
            }, 150);
            return _ => clearTimeout(interval);
        },
        [data, index, active]
    );

    if (isLoading) return "Loading...";

    if (error) return "Error: " + error.message;

    const offsetX = data.map((v) => v.head[0]).reduce((a, b) => Math.min(a, b));
    const offsetY = data.map((v) => v.head[1]).reduce((a, b) => Math.min(a, b));
    
    const tails =  removeDuplicates(data.slice(0, Math.max(index+1, 0)).map((v) => v.tail));
    return (
        <div style={{background: "#333333", color: "#eeeeee"}}>
          <div style={{textAlign: "center", fontSize: 21}}>
            <input style={{height: "3em", width: "6em"}} type="button"
                   value={active ? "Stop" : "Start"} onClick={(_) => {toggleActive();}}/>
          </div>
          <div style={{textAlign: "center", fontSize: 64}}>
            <span style={{fontSize: 32}}>Index: {1+index} of {data.length}</span>&nbsp;|&nbsp;
            <span style={{fontSize: 32}}>Tail gone into {tails.length} unique places</span>
          </div>
          <RopeSnapshot rope={data[index]} offsetX={offsetX} offsetY={offsetY} tails={tails}/>
        </div>
    );
}

export default App;
