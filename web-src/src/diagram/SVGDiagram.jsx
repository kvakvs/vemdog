import {SequenceDiagram} from 'svg-sequence-diagram';
import {createRef, useEffect, useMemo} from "react";
import {TransformComponent, TransformWrapper} from "react-zoom-pan-pinch";
import {buildFromEvents} from "./diagramGen";

export const SVGDiagram = ({data, shownPids}) => {
    const textRepresentation = useMemo(() => buildFromEvents(data, shownPids), [data, shownPids]);
    const diagramContainerRef = createRef();

    const diagram = useMemo(() => new SequenceDiagram(""), []);
    useEffect(() => {
        console.log("diagram regenerated", textRepresentation)
        diagram.set(textRepresentation);
        diagram.setContainer(diagramContainerRef.current);
    }, [textRepresentation, diagram, diagramContainerRef]);

    return <TransformWrapper initialScale={1}
                             initialPositionX={0}
                             initialPositionY={0}>
        <TransformComponent>
            <div style={{width: "100vw", height: "100vh"}} ref={diagramContainerRef}/>
        </TransformComponent>
    </TransformWrapper>
}