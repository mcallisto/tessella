[![Build Status][travis-badge]][travis-link] [![Maven Central][maven-badge]][maven-link] [![Codacy Badge][codacy-badge]][codacy-link]

# Tilings by regular polygons

This Scala library helps working with finite unit-regular-polygon tessellations of a flat surface, a classical theme in the wider field of [tessellations](https://en.wikipedia.org/wiki/Tessellation) (or _tilings_). See a [mathematical definition of tiling](docs/tiling-definition.md) for a more accurate notion of the chosen constraints.

> ![(▲.■.⬣.■)](docs/(▲.■.⬣.■).svg)
> _Finite set of the (▲.■.⬣.■) Archimedean (1-uniform) tiling_

## Tessellation as graph

Each tessellation is internally described as an [undirected graph](https://en.wikipedia.org/wiki/Graph_(discrete_mathematics)#Undirected_graph), where:

*   each **node** of the graph is a _**vertex**_ of a polygon and it is represented by a unique `Int`
*   each **edge** of the graph is a _**side**_ of a polygon

The graph describing the tessellation is a `Tiling` object and can be created through algorithms.
Many of them, exploiting linear symmetries, are already available from the `creation` subpackage.

An undirected graph is not necessarily a valid `Tiling`, see [graph validation](docs/graph-validation.md).

Graphs are seamless Scala collections thanks to the excellent [Graph for Scala](https://scala-graph.org/) library by Peter Empen.

## How to

### Use

#### From a Mill project

```scala
import mill._, scalalib._

object foo extends ScalaModule {
  def ivyDeps = Agg(
    ivy"vision.id::tessella:0.1.0"
  )
}
```

#### From an SBT project

```scala
libraryDependencies += "vision.id" % "tessella" % "0.1.0"
```

### Test

The library is built with [Mill](http://www.lihaoyi.com/mill).

1.  [Install](http://www.lihaoyi.com/mill/#installation) Mill
2.  Open a terminal and `cd` to the repo directory
3.  Use the `mill jvm.test` command to run all tests
4.  Or use the `mill jvm.test.one [testClassName]` command to run a single test class

### Draw a tessellation

Each node of the tessellation can be `Map`ped to a cartesian point with the `Tiling.toTessellMap` method. The tessellation can then be rendered as a layered SVG image with the `Draw(…)` method of the `SVG` trait.

The lowest node is always at coordinates `(0.0, 0.0)` and the second lowest at `(1.0, 0.0)`.

On top of the edge layer, the following optional additions are possible:

*   node labels
    > ![(⬟².10)_label](docs/(⬟².10)_label.svg)

*   perimeter polygon
    > ![(⬟².10)_perimeter](docs/(⬟².10)_perimeter.svg)

*   filled polygons coloured according to number of sides
    > ![(⬟².10)_filled](docs/(⬟².10)_filled.svg)

*   full vertices coloured according to type of adjacent polygons (gonality)
    > ![(▲⁶; (⬣³)²; (▲².⬣²)²)](docs/(▲⁶;(⬣³)²;(▲².⬣²)²).svg)

### Redraw the images in the /docs folder

Uncomment the first test in the `outputTest` class and run it.

### Draw several examples of algorithmic tessellations

Uncomment the second test in the `outputTest` class, run it and view them ordered in the temporary `out/jvm/test/myAlgos` folder.

## Vertex

For the library methods a given vertex is described by the ordered adjacent regular p-gons joined at the vertex.

Example: `(▲.■)`

_Note:_ description can be shortened if consecutive identical elements, where `(⬣.⬣.▲)` becomes `(⬣².▲)`

A vertex is **full** when the adjacent regular p-gons join to exactly complete a full circle.

Example: `(▲.■.⬣.■)` or `(■.⬣.12)`

In a finite tessellation:

*   all vertices on the perimeter are NOT full
*   all other vertices are full

### 14 p-gons

There are only 21 possible combinations of regular p-gons for the full vertex and they use only 14 different p-gons:

1.  **Triangle**, **3** sides, alt symbol `▲`
2.  **Square**, **4** sides, alt symbol `■`
3.  **Pentagon**, **5** sides, alt symbol `⬟`
4.  **Hexagon**, **6** sides, alt symbol `⬣`
5.  **Eptagon**, **7** sides
6.  **Octagon**, **8** sides, alt symbol `⯃`
7.  **Ennagon**, **9** sides
8.  **Decagon**, **10** sides
9.  **Dodecagon**, **12** sides
10. **Gon15**, **15** sides
11. **Gon18**, **18** sides
12. **Icosagon**, **20** sides
13. **Gon24**, **24** sides
14. **Gon42**, **42** sides

## Infinite tiling

Can be named after the different full vertices it's composed of.

Example: `(▲.■.⬣.■)` or `(▲⁶; ▲².■.▲.■)`

[travis-badge]: https://travis-ci.org/mcallisto/tessella.svg
[travis-link]: https://travis-ci.org/mcallisto/tessella
[maven-badge]: https://maven-badges.herokuapp.com/maven-central/vision.id/tessella_2.12/badge.svg
[maven-link]: https://maven-badges.herokuapp.com/maven-central/vision.id/tessella_2.12
[codacy-badge]: https://api.codacy.com/project/badge/Grade/c9a888d7249943a3b5b82e64fdcc7a52
[codacy-link]: https://www.codacy.com/app/mcallisto/tessella?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=mcallisto/tessella&amp;utm_campaign=Badge_Grade