# Tilings by regular polygons
This Scala library helps working with finite unit-regular-polygon tessellations of a flat surface, a classical theme in the wider field of [tessellations](https://en.wikipedia.org/wiki/Tessellation) (or _tilings_). See the [Mathematical definition of tiling](docs/tiling-definition.md) for a more accurate notion of the chosen constraints.

> ![(▲.■.⬣.■)](docs/(▲.■.⬣.■).svg)
> _Finite set of the (▲.■.⬣.■) Archimedean (1-uniform) tiling_

## Tessellation as graph
Each tessellation is internally described as an [undirected graph](https://en.wikipedia.org/wiki/Graph_(discrete_mathematics)#Undirected_graph), where:

* each **node** of the graph is a _**vertex**_ of a polygon and it is represented by a unique `Int`
* each **edge** of the graph is a _**side**_ of a polygon

The graph describing the tessellation can be created through algorithms.
Many of them, exploiting linear symmetries, are already available from the `Reticulate` and `Net` traits.

Not all undirected graphs are legitimate tessellations, see [Graph validation](docs/graph-validation.md).

Graphs are seamless Scala collections thanks to the excellent [Graph for Scala](https://scala-graph.org/) library by Peter Empen.

## How to

### Test
The library is built with [Mill](http://www.lihaoyi.com/mill).

1. [Install](http://www.lihaoyi.com/mill/#installation) Mill
2. Open a terminal and `cd` to the repo directory
3. Use the `mill jvm.test` command to run all tests

### Draw a tessellation
Each node of the tessellation can be `Map`ped to a cartesian point with the `TessellGraph.toTessellMap` method. The tessellation can then be rendered as a layered SVG image with the `Draw(…)` method of the `SVG` trait.

The lowest node is always at coordinates `(0.0, 0.0)` and the second lowest at `(1.0, 0.0)`.

On top of the edge layer, the following optional additions are possible:
* node labels
> ![(⬟².10)_label](docs/(⬟².10)_label.svg)
* perimeter polygon
> ![(⬟².10)_perimeter](docs/(⬟².10)_perimeter.svg)
* filled polygons coloured according to number of sides
> ![(⬟².10)_filled](docs/(⬟².10)_filled.svg)

### Redraw the images in the docs folder

Use the `mill jvm.run output docs` command.

### Draw several examples of algorithmic tessellations

Use the `mill jvm.run output algos` command and view them ordered in the temporary `out/jvm/myAlgos` folder.

## Vertex
For the library methods a given vertex is described by the ordered adjacent regular p-gons joined at the vertex.

Example: `(▲.■)`

_Note:_ description can be shortened if consecutive identical elements, where `(⬣.⬣.▲)` becomes `(⬣².▲)`

A vertex is **full** when the adjacent regular p-gons join to exactly complete a full circle.

Example: `(▲.■.⬣.■)` or `(■.⬣.12)`

In a finite tessellation:
* all vertices on the perimeter are NOT full
* all other vertices are full

### 14 p-gons
There are only 21 possible combinations of regular p-gons for the full vertex and they use only 14 different p-gons:

1. **Triangle**, **3** sides, alt symbol `▲`
2. **Square**, **4** sides, alt symbol `■`
3. **Pentagon**, **5** sides, alt symbol `⬟`
4. **Hexagon**, **6** sides, alt symbol `⬣`
5. **Eptagon**, **7** sides
6. **Octagon**, **8** sides, alt symbol `⯃`
7. **Ennagon**, **9** sides
8. **Decagon**, **10** sides
9. **Dodecagon**, **12** sides
10. **Gon15**, **15** sides
11. **Gon18**, **18** sides
12. **Icosagon**, **20** sides
13. **Gon24**, **24** sides
14. **Gon42**, **42** sides


## Infinite tiling
Can be named after the different full vertices it's composed of.

Example: `(▲.■.⬣.■)` or `(▲⁶; ▲².■.▲.■)`