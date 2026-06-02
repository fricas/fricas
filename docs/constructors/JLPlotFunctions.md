# JLPlotFunctions

> **Kind**: Package &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jplot.spad#L1)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

low level plotting functions using the Plots package for Julia.

**JLPlotFunctions is a package constructor**  
**Abbreviation for JLPlotFunctions is JPLOT**  
**This constructor is exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 jlBar : (JLFloat64Vector, JLFloat64Vector) -> Void     jlBar! : (JLFloat64Vector, JLFloat64Vector) -> Void
 jlClosePlots : () -> Void                              jlDensity : JLFloat64Vector -> Void
 jlDensity! : JLFloat64Vector -> Void                   jlPlot : (JLFloat64Vector, JLFloat64Vector) -> Void
 jlPlotTitle! : String -> Void                          jlPlotXlabel! : String -> Void
 jlPlotXlims! : (JLFloat64, JLFloat64) -> Void          jlPlotYlabel! : String -> Void
 jlPlotYlims! : (JLFloat64, JLFloat64) -> Void          jlPlotZlabel! : String -> Void
 jlPlotZlims! : (JLFloat64, JLFloat64) -> Void          jlRange : Segment(Float) -> JLFloat64Vector
 jlSavePlot : String -> Void                            jlSetPlotsBackend! : String -> Void
 jlContour : (JLFloat64Vector, JLFloat64Vector, JLFloat64Matrix) -> Void
 jlContour : (((DoubleFloat, DoubleFloat) -> DoubleFloat), Segment(Float), Segment(Float)) -> Void
 jlContour! : (JLFloat64Vector, JLFloat64Vector, JLFloat64Matrix) -> Void
 jlContour! : (((DoubleFloat, DoubleFloat) -> DoubleFloat), Segment(Float), Segment(Float)) -> Void
 jlContourf : (JLFloat64Vector, JLFloat64Vector, JLFloat64Matrix) -> Void
 jlContourf : (((DoubleFloat, DoubleFloat) -> DoubleFloat), Segment(Float), Segment(Float)) -> Void
 jlContourf! : (JLFloat64Vector, JLFloat64Vector, JLFloat64Matrix) -> Void
 jlContourf! : (((DoubleFloat, DoubleFloat) -> DoubleFloat), Segment(Float), Segment(Float)) -> Void
 jlCurves : (JLFloat64Vector, JLFloat64Vector) -> Void
 jlCurves! : (JLFloat64Vector, JLFloat64Vector) -> Void
 jlPlot : (JLFloat64Vector, JLFloat64Vector, JLFloat64Vector) -> Void
 jlPlot : ((DoubleFloat -> DoubleFloat), Segment(Float)) -> Void
 jlPlot : (((DoubleFloat, DoubleFloat) -> DoubleFloat), Segment(Float), Segment(Float)) -> Void
 jlPlot! : (JLFloat64Vector, JLFloat64Vector) -> Void
 jlPlot! : (JLFloat64Vector, JLFloat64Vector, JLFloat64Vector) -> Void
 jlPlot! : ((DoubleFloat -> DoubleFloat), Segment(Float)) -> Void
 jlPlot! : (((DoubleFloat, DoubleFloat) -> DoubleFloat), Segment(Float), Segment(Float)) -> Void
 jlRange : (Segment(Float), PositiveInteger) -> JLFloat64Vector
 jlSurface : (JLFloat64Vector, JLFloat64Vector, JLFloat64Matrix) -> Void
 jlSurface : (((DoubleFloat, DoubleFloat) -> DoubleFloat), Segment(Float), Segment(Float)) -> Void
 jlSurface! : (JLFloat64Vector, JLFloat64Vector, JLFloat64Matrix) -> Void
 jlSurface! : (((DoubleFloat, DoubleFloat) -> DoubleFloat), Segment(Float), Segment(Float)) -> Void
 jlWireframe : (JLFloat64Vector, JLFloat64Vector, JLFloat64Matrix) -> Void
 jlWireframe : (((DoubleFloat, DoubleFloat) -> DoubleFloat), Segment(Float), Segment(Float)) -> Void
 jlWireframe! : (JLFloat64Vector, JLFloat64Vector, JLFloat64Matrix) -> Void
 jlWireframe! : (((DoubleFloat, DoubleFloat) -> DoubleFloat), Segment(Float), Segment(Float)) -> Void
```

## Operations added

### `jlBar` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jplot.spad#L47)\]

jlBar(x,y) draws a bar plot of y vs x.

- **Signature**: `(JLFloat64Vector,JLFloat64Vector)->Void`

### `jlBar!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jplot.spad#L49)\]

jlBar!(x,y) draws a bar plot of y vs x on the existing graph object.

- **Signature**: `(JLFloat64Vector,JLFloat64Vector)->Void`

### `jlClosePlots` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jplot.spad#L20)\]

jlClosePlots() closes all plot windows.

- **Signature**: `()->Void`

### `jlContour` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jplot.spad#L66)\]

jlContour(x,y,mat) draws the contour plot of surface mat.

- **Signature**: `(JLFloat64Vector,JLFloat64Vector,JLFloat64Matrix)->Void`

jlContour(f, a..b, c..d) draws the contour plot of y = f(x,y) as x ranges from min(a, b) to max(a, b) and y ranges from min(c, d) to max(c, d).

- **Signature**: `((DoubleFloat,DoubleFloat)->DoubleFloat,Segment(Float),Segment(Float))->Void`

### `jlContour!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jplot.spad#L68)\]

jlContour!(x,y,mat) draws the contour plot of surface mat on the existing graph object.

- **Signature**: `(JLFloat64Vector,JLFloat64Vector,JLFloat64Matrix)->Void`

jlContour!(f, a..b, c..d) draws the contour plot of y = f(x,y) as x ranges from min(a, b) to max(a, b) and y ranges from min(c, d) to max(c, d) on the existing graph object.

- **Signature**: `((DoubleFloat,DoubleFloat)->DoubleFloat,Segment(Float),Segment(Float))->Void`

### `jlContourf` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jplot.spad#L71)\]

jlContourf(x,y,mat) draws the contour plot of surface mat. Fill area between contours.

- **Signature**: `(JLFloat64Vector,JLFloat64Vector,JLFloat64Matrix)->Void`

jlContourf(f, a..b, c..d) draws the contour plot of y = f(x,y) as x ranges from min(a, b) to max(a, b) and y ranges from min(c, d) to max(c, d). Fill area between contours.

- **Signature**: `((DoubleFloat,DoubleFloat)->DoubleFloat,Segment(Float),Segment(Float))->Void`

### `jlContourf!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jplot.spad#L74)\]

jlContourf!(x,y,mat) draws the contour plot of surface mat on the existing graph object. Fill area between contours.

- **Signature**: `(JLFloat64Vector,JLFloat64Vector,JLFloat64Matrix)->Void`

jlContourf!(f, a..b, c..d) draws the contour plot of y = f(x,y) as x ranges from min(a, b) to max(a, b) and yranges from min(c, d) to max(c, d) on the existing graph object. Fill area between contours.

- **Signature**: `((DoubleFloat,DoubleFloat)->DoubleFloat,Segment(Float),Segment(Float))->Void`

### `jlCurves` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jplot.spad#L39)\]

jlCurves(x,y) draws a Bezier plot curve from (x.1,y.1) to (x.end,y.end) with control points (x.2,y.2),..., (x.end-1,y.end-1) and draws it.

- **Signature**: `(JLFloat64Vector,JLFloat64Vector)->Void`

### `jlCurves!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jplot.spad#L43)\]

jlCurves!(x,y) draws a Bezier plot curve from (x.1,y.1) to (x.end,y.end) with control points (x.2,y.2),..., (x.end-1,y.end-1) and draws it on the existing graph object.

- **Signature**: `(JLFloat64Vector,JLFloat64Vector)->Void`

### `jlDensity` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jplot.spad#L29)\]

jlDensity(x) draws a line plot of a kernel density estimate of x.

- **Signature**: `(JLFloat64Vector)->Void`

### `jlDensity!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jplot.spad#L31)\]

jlDensity!(x) draws a line plot of a kernel density estimate of x on the existing graph object.

- **Signature**: `(JLFloat64Vector)->Void`

### `jlPlot` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jplot.spad#L34)\]

jlPlot(x,y) draws a plot using elements of x and y.

- **Signature**: `(JLFloat64Vector,JLFloat64Vector)->Void`

jlPlot(x,y,z) draws a plot using elements of x, y and z.

- **Signature**: `(JLFloat64Vector,JLFloat64Vector,JLFloat64Vector)->Void`

jlPlot(f, a..b) draws the graph of y = f(x) as x ranges from min(a, b) to max(a, b). For example:

**Example**:
```fricas
jl Plot(x+->sinc(x),-15..15)
```

- **Signature**: `((DoubleFloat)->DoubleFloat,Segment(Float))->Void`

jlPlot(f, a..b, c..d) draws the graph of z = f(x, y) as x ranges from min(a, b) to max(a, b) and y ranges from min(c, d) to max(c, d)

- **Signature**: `((DoubleFloat,DoubleFloat)->DoubleFloat,Segment(Float),Segment(Float))->Void`

### `jlPlot!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jplot.spad#L36)\]

jlPlot!(x,y) draws a plot using elements of x and y on the existing graph object.

- **Signature**: `(JLFloat64Vector,JLFloat64Vector)->Void`

jlPlot!(x,y,z) draws a plot using elements of x, y and z on the existing graph object.

- **Signature**: `(JLFloat64Vector,JLFloat64Vector,JLFloat64Vector)->Void`

jlPlot!(f, a..b) adds the graph of y = f(x) as x ranges from min(a, b) to max(a, b) to the existing graph object.

- **Signature**: `((DoubleFloat)->DoubleFloat,Segment(Float))->Void`

jlPlot!(f, a..b, c..d) adds the graph of z = f(x, y) as x ranges from min(a, b) to max(a, b) and y ranges from min(c, d) to max(c, d) on the existing graph object.

- **Signature**: `((DoubleFloat,DoubleFloat)->DoubleFloat,Segment(Float),Segment(Float))->Void`

### `jlPlotTitle!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jplot.spad#L77)\]

jlPlotTitle!(title) draws `title' as plot title in the current plot. For example: jlPlotTitle! "sin(x)" if you have the LaTeXStrings Julia package 

**Example**:
```fricas
jlPlotTitle! jlLatex latex sin(x) For simple expressions title will automatically be enclosed between $. So use:
```

**Example**:
```fricas
jlPlotTitle! jlLatex "sin(x)" For normal text an d equations insert dollar signs $ as needed.
```

**Example**:
```fricas
jlPlotTitle! jlLatex "Plot: $sin(x)$"
```

- **Signature**: `(String)->Void`

### `jlPlotXlabel!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jplot.spad#L95)\]

jlPlotXlabel!(str) sets x label. See jlPlotTitle from JLPlotFunctions for more informations on the different supported formats.

- **Signature**: `(String)->Void`

### `jlPlotXlims!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jplot.spad#L89)\]

jlPlotXlims!(x1,x2) sets x plot limits.

- **Signature**: `(JLFloat64,JLFloat64)->Void`

### `jlPlotYlabel!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jplot.spad#L99)\]

jlPlotYlabel!(str) sets y label. See jlPlotTitle from JLPlotFunctions for more informations on the different supported formats.

- **Signature**: `(String)->Void`

### `jlPlotYlims!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jplot.spad#L91)\]

jlPlotYlims!(y1,y2) sets y plot limits.

- **Signature**: `(JLFloat64,JLFloat64)->Void`

### `jlPlotZlabel!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jplot.spad#L103)\]

jlPlotZlabel!(str) sets z label. See jlPlotTitle from JLPlotFunctions for more informations on the different supported formats.

- **Signature**: `(String)->Void`

### `jlPlotZlims!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jplot.spad#L93)\]

jlPlotZlims!(z1,z2) sets z plot limits.

- **Signature**: `(JLFloat64,JLFloat64)->Void`

### `jlRange` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jplot.spad#L22)\]

jlRange(range) is a utility function that returns a JLFloat64Vector with 250 points from low(range) to high(range).

- **Signature**: `(Segment(Float))->JLFloat64Vector`

jlRange(range, n) is a utility function that returns a JLFloat64Vector with n points from low(range) to high(range).

- **Signature**: `(Segment(Float),PositiveInteger)->JLFloat64Vector`

### `jlSavePlot` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jplot.spad#L167)\]

jlSavePlot(file) save the current plot in file. Supported formats: png (default), PDF, svg and more. See https://docs.juliaplots.org/latest/output/#savefig-/-format

- **Signature**: `(String)->Void`

### `jlSetPlotsBackend!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jplot.spad#L16)\]

jlSetPlotsBackend!(backend) sets the backend for plotting. The available backends are "gr", gaston", "plotly", "plotlyjs", "pythonplot", "pgfplotsx", "unicodeplots" and "inspectdr".

- **Signature**: `(String)->Void`

### `jlSurface` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jplot.spad#L61)\]

jlSurface(x,y,mat) draws the plot surface of mat.

- **Signature**: `(JLFloat64Vector,JLFloat64Vector,JLFloat64Matrix)->Void`

jlSurface(f, a..b, c..d) draws the surface of y = f(x,y) as x ranges from min(a, b) to max(a, b) and y rangesfrom min(c, d) to max(c, d).

- **Signature**: `((DoubleFloat,DoubleFloat)->DoubleFloat,Segment(Float),Segment(Float))->Void`

### `jlSurface!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jplot.spad#L63)\]

jlSurface!(x,y,mat) draws the plot surface of mat on the existing graph object.

- **Signature**: `(JLFloat64Vector,JLFloat64Vector,JLFloat64Matrix)->Void`

jlSurface!(f, a..b, c..d) draws the surface of y = f(x,y) as x ranges from min(a, b) to max(a, b) and y ranges from min(c, d) to max(c, d) on the existing graph object.

- **Signature**: `((DoubleFloat,DoubleFloat)->DoubleFloat,Segment(Float),Segment(Float))->Void`

### `jlWireframe` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jplot.spad#L56)\]

jlWireframe(x,y,mat) draws the wireframe surface of mat.

- **Signature**: `(JLFloat64Vector,JLFloat64Vector,JLFloat64Matrix)->Void`

jlWireframe(f, a..b, c..d) draws the wireframe of y = f(x,y) as x ranges from min(a, b) to max(a, b) and y ranges from min(c, d) to max(c, d).

- **Signature**: `((DoubleFloat,DoubleFloat)->DoubleFloat,Segment(Float),Segment(Float))->Void`

### `jlWireframe!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jplot.spad#L58)\]

jlWireframe!(x,y,mat) draws the wireframe surface of mat on the existing graph object.

- **Signature**: `(JLFloat64Vector,JLFloat64Vector,JLFloat64Matrix)->Void`

jlWireframe!(f, a..b, c..d) draws add the wireframe of y = f(x,y) as x ranges from min(a, b) to max(a, b) andy ranges from min(c, d) to max(c, d) on the existing graph object.

- **Signature**: `((DoubleFloat,DoubleFloat)->DoubleFloat,Segment(Float),Segment(Float))->Void`
---
[Back to Index](../index.md)
