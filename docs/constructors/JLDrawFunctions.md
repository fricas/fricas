# JLDrawFunctions

> **Kind**: Package &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jplot.spad#L410)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

JLDrawFunctions provides top level functions for drawing graphics of expressions.

**JLDrawFunctions(Ex: Join(ConvertibleTo(InputForm),SetCategory)) is a package constructor**  
**Abbreviation for JLDrawFunctions is JDRAW**  
**This constructor is exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 jlPlot : (Ex, SegmentBinding(Float)) -> Void           jlPlot! : (Ex, SegmentBinding(Float)) -> Void
 jlContour : (Ex, SegmentBinding(Float), SegmentBinding(Float)) -> Void
 jlContour! : (Ex, SegmentBinding(Float), SegmentBinding(Float)) -> Void
 jlContourf : (Ex, SegmentBinding(Float), SegmentBinding(Float)) -> Void
 jlContourf! : (Ex, SegmentBinding(Float), SegmentBinding(Float)) -> Void
 jlPlot : (Ex, SegmentBinding(Float), SegmentBinding(Float)) -> Void
 jlPlot! : (Ex, SegmentBinding(Float), SegmentBinding(Float)) -> Void
 jlSurface : (Ex, SegmentBinding(Float), SegmentBinding(Float)) -> Void
 jlSurface! : (Ex, SegmentBinding(Float), SegmentBinding(Float)) -> Void
 jlWireframe : (Ex, SegmentBinding(Float), SegmentBinding(Float)) -> Void
 jlWireframe! : (Ex, SegmentBinding(Float), SegmentBinding(Float)) -> Void
```

## Operations added

### `jlContour` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jplot.spad#L474)\]

jlContour(f(x, y), x = a..b, y = c..d) draws the surface of z = f(x, y) as x ranges from min(a, b) to to max(a, b) and y ranges from min(c, d) to max(c, d). f(x, y) appears as the title.

- **Signature**: `(Ex,SegmentBinding(Float),SegmentBinding(Float))->Void`

### `jlContour!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jplot.spad#L479)\]

jlContour!(f(x, y), x = a..b, y = c..d) adds the surface of z = f(x, y) as x ranges from min(a, b) to max(a, b) and y ranges from min(c, d) to max(c, d) on the existing graph window. f(x, y) appears as the title.

- **Signature**: `(Ex,SegmentBinding(Float),SegmentBinding(Float))->Void`

### `jlContourf` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jplot.spad#L485)\]

jlContourf(f(x, y), x = a..b, y = c..d) draws the surface of z = f(x, y) as x ranges from min(a, b) to to max(a, b) and y ranges from min(c, d) to max(c, d). f(x, y) appears as the title. Fills area between contours.

- **Signature**: `(Ex,SegmentBinding(Float),SegmentBinding(Float))->Void`

### `jlContourf!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jplot.spad#L491)\]

jlContourf!(f(x, y), x = a..b, y = c..d) adds the surface of z = f(x, y) as x ranges from min(a, b) to max(a,b) and y ranges from min(c, d) to max(c, d) on the existing graph window. f(x, y) appears as the title. Fillsarea between contours.

- **Signature**: `(Ex,SegmentBinding(Float),SegmentBinding(Float))->Void`

### `jlPlot` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jplot.spad#L429)\]

jlPlot(f(x), x = a..b) draws the graph of y = f(x) as x ranges from min(a, b) to max(a, b). f(x) appears as the title.

- **Signature**: `(Ex,SegmentBinding(Float))->Void`

jlPlot(f(x, y), x = a..b, y = c..d) draws the graph of z = f(x, y) as x ranges from min(a, b) to to max(a, b)and y ranges from min(c, d) to max(c, d). f(x, y) appears as the title.

- **Signature**: `(Ex,SegmentBinding(Float),SegmentBinding(Float))->Void`

### `jlPlot!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jplot.spad#L433)\]

jlPlot!(f(x), x = a..b) adds the graph of y = f(x) as x ranges from min(a, b) to max(a, b) on the existing graphic window. f(x) appears as the title.

- **Signature**: `(Ex,SegmentBinding(Float))->Void`

jlPlot!(f(x, y), x = a..b, y = c..d) adds the graph of z = f(x, y) as x ranges from min(a, b) to max(a, b) and y ranges from min(c, d) to max(c, d) on the existing graph window. f(x, y) appears as the title.

- **Signature**: `(Ex,SegmentBinding(Float),SegmentBinding(Float))->Void`

### `jlSurface` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jplot.spad#L463)\]

jlSurface(f(x, y), x = a..b, y = c..d) draws the surface of z = f(x, y) as x ranges from min(a, b) to to max(a, b) and y ranges from min(c, d) to max(c, d). f(x, y) appears as the title.

- **Signature**: `(Ex,SegmentBinding(Float),SegmentBinding(Float))->Void`

### `jlSurface!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jplot.spad#L468)\]

jlSurface!(f(x, y), x = a..b, y = c..d) adds the surface of z = f(x, y) as x ranges from min(a, b) to max(a, b) and y ranges from min(c, d) to max(c, d) on the existing graph window. f(x, y) appears as the title.

- **Signature**: `(Ex,SegmentBinding(Float),SegmentBinding(Float))->Void`

### `jlWireframe` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jplot.spad#L452)\]

jlWireframe(f(x, y), x = a..b, y = c..d) draws the wireframe of z = f(x, y) as x ranges from min(a, b) to to max(a, b) and y ranges from min(c, d) to max(c, d); f(x, y) appears as the title.

- **Signature**: `(Ex,SegmentBinding(Float),SegmentBinding(Float))->Void`

### `jlWireframe!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jplot.spad#L457)\]

jlWireframe!(f(x, y), x = a..b, y = c..d) adds the wireframe of z = f(x, y) as x ranges from min(a, b) to max(a, b) and y ranges from min(c, d) to max(c, d) on the existing graph window. f(x, y) appears as the title.

- **Signature**: `(Ex,SegmentBinding(Float),SegmentBinding(Float))->Void`
---
[Back to Index](../index.md)
