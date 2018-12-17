# Graph validation

A `Tiling` instance, representing a valid tessellation, is a graph that:

* cannot have nodes connected to less than other 2 nodes (with degree < 2), they would represent an external point (if degree = 0) or an open polygon (if degree = 1)

> ![1-degree node](constraints/1-degree.svg)
> _Node 4 is 1-degree_

* cannot have nodes connected to more than other 6 nodes (with degree > 6), they would represent a vertex with more than 6 adjacent polygons, and this is impossible since the polygon with the smaller exterior angle, the regular triangle, fills the full angle with 6 units

> ![7-degree node](constraints/7-degree.svg)
> ![7-degree node as standard graph](constraints/7-degree.png)
> _Node 1 is 7-degree_

* must be connected

> ![disconnected graph](constraints/disconnected.svg)
> _Nodes 1, 2, 3 are not connected to nodes 4, 5, 6_

* cannot have not adjacent polygons at the same vertex

> ![non adjacent p-gons at the same vertex](constraints/nonAdjacent.svg)
> _Polygons at the same vertex (node 2) are not adjacent_

* cannot have adjacent polygons at the same vertex making more than a full circle

> ![more than full circle at the same vertex](constraints/moreThanFull.svg)
> ![more than full circle at the same vertex as standard graph](constraints/moreThanFull.png)
> _Three squares and a regular pentagon at the same vertex (node 3) make more than a full circle_

> ![area overlapping1](constraints/areaOverlap1.svg)
> ![area overlapping1 as standard graph](constraints/areaOverlap1.png)
> _Three squares and a regular pentagon at the same vertex (node 3) overlap_

* cannot have areas overlapping

> ![area overlapping2](constraints/areaOverlap2.svg)
> ![area overlapping2 as standard graph](constraints/areaOverlap2.png)
> _Area overlapping_

> ![area overlapping3](constraints/areaOverlap3.svg)
> ![area overlapping3 as standard graph](constraints/areaOverlap3.png)
> _Area and sides overlapping, edges 2\~5 with 21\~19 and 3\~6 with 22\~17_

* cannot have sides overlapping

> ![side overlapping](constraints/sideOverlap.svg)
> ![side overlapping as standard graph](constraints/sideOverlap.png)
> _Sides overlapping, edges 2\~5 with 24\~19_

* cannot have vertices overlapping

> ![vertex overlapping](constraints/vertexOverlap.svg)
> ![vertex overlapping as standard graph](constraints/vertexOverlap.png)
> _Vertex overlapping, node 6 and node 15 at the same position_

* cannot have inside gaps, they would be considered non-regular unit polygons

> ![gap](constraints/gap.svg)
> ![gap as standard graph](constraints/gap.png)
> _Gap_
