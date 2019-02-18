# Graph validation

A `Tiling` instance that represents a valid tessellation is a graph with the following properties:

* all nodes connected to at least 2 other nodes (degree >= 2) - otherwise, a node of degree 0 would represent an external point and a node of degree 1 an open polygon

> ![1-degree node](constraints/1-degree.svg)
>
> _Invalid since node 4 has degree 1_

* all nodes connected to at most 6 other nodes (degree <= 6) - otherwise, a node would have 6 adjacent polygons (impossible since the polygon with the smaller exterior angle, the regular triangle, fills the full angle with 6 units)

> ![7-degree node](constraints/7-degree.svg)
> ![7-degree node as standard graph](constraints/7-degree.png)
>
> _Invalid since node 1 has degree 7_

* the graph is connected

> ![disconnected graph](constraints/disconnected.svg)
>
> _Invalid since nodes 1, 2, 3 are not connected to nodes 4, 5, 6_

* there exist no adjacent polygons at the same vertex

> ![non adjacent p-gons at the same vertex](constraints/nonAdjacent.svg)
>
> _Invalid due to polygons at node 2_

* there exist no adjacent polygons at the same vertex making more than a full circle

> ![more than full circle at the same vertex](constraints/moreThanFull.svg)
> ![more than full circle at the same vertex as standard graph](constraints/moreThanFull.png)
>
> _Invalid due to three squares and a regular pentagon at node 3 that make more than a full circle_

> ![area overlapping1](constraints/areaOverlap1.svg)
> ![area overlapping1 as standard graph](constraints/areaOverlap1.png)
>
> _Invalid due to overlapping squares and a regular pentagon at node 3_

* there exist no overlapping areas

> ![area overlapping2](constraints/areaOverlap2.svg)
> ![area overlapping2 as standard graph](constraints/areaOverlap2.png)
>
> _Invalid due to overlapping area_

> ![area overlapping3](constraints/areaOverlap3.svg)
> ![area overlapping3 as standard graph](constraints/areaOverlap3.png)
>
> _Invalid due to overlapping area and sides: edges 2\~5 with 21\~19 and 3\~6 with 22\~17_

* there exist no overlapping sides

> ![side overlapping](constraints/sideOverlap.svg)
> ![side overlapping as standard graph](constraints/sideOverlap.png)
>
> _Invalid due to the overlapping sides 2\~5 and 24\~19_

* there exist no overlapping vertices

> ![vertex overlapping](constraints/vertexOverlap.svg)
> ![vertex overlapping as standard graph](constraints/vertexOverlap.png)
>
> _Invalid due to the overlapping vertexes 6 and 15_

* there exist no "inside gaps", they would be considered non-regular unit polygons

> ![gap](constraints/gap.svg)
> ![gap as standard graph](constraints/gap.png)
>
> _Invalid due to a gap_
