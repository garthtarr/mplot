# Plot diagnostics for a bglmnet object

A plot method to visualise the results of a `bglmnet` object.

## Usage

``` r
# S3 method for class 'bglmnet'
plot(
  x,
  highlight,
  interactive = FALSE,
  classic = NULL,
  tag = NULL,
  shiny = FALSE,
  which = c("vip", "boot", "boot_size"),
  width = 800,
  height = 400,
  fontSize = 12,
  left = 50,
  top = 30,
  chartWidth = "60%",
  chartHeight = "80%",
  axisTitlesPosition = "out",
  dataOpacity = 0.5,
  options = NULL,
  hAxis.logScale = TRUE,
  ylim,
  text = FALSE,
  backgroundColor = "transparent",
  legend.position = "right",
  jitterk = 0.1,
  srt = 45,
  max.circle = 15,
  min.prob = 0.1,
  ...
)
```

## Arguments

- x:

  `bglmnet` object, the result of
  [`bglmnet`](https://garthtarr.github.io/mplot/reference/bglmnet.md)

- highlight:

  the name of a variable that will be highlighted.

- interactive:

  logical. If `interactive=TRUE` a googleVis plot is provided instead of
  the base graphics plot. Default is `interactive=FALSE`.

- classic:

  logical. Depricated. If `classic=TRUE` a base graphics plot is
  provided instead of a googleVis plot. For now specifying `classic`
  will overwrite the default `interactive` behaviour, though this is
  likely to be removed in the future.

- tag:

  Default NULL. Name tag of the objects to be extracted from a gvis
  (googleVis) object.

  The default tag for is NULL, which will result in R opening a browser
  window. Setting `tag='chart'` or setting
  `options(gvis.plot.tag='chart')` is useful when googleVis is used in
  scripts, like knitr or rmarkdown.

- shiny:

  Default FALSE. Set to TRUE when using in a shiny interface.

- which:

  a vector specifying the plots to be output. Variable inclusion type
  plots `which = "vip"` or plots where the size of the point
  representing each model is proportional to selection probabilities by
  model size `which = "boot_size"` or by penalty paramter
  `which = "boot"`.

- width:

  Width of the googleVis chart canvas area, in pixels. Default: 800.

- height:

  Height of the googleVis chart canvas area, in pixels. Default: 400.

- fontSize:

  font size used in googleVis chart. Default: 12.

- left:

  space at left of chart (pixels?). Default: "50".

- top:

  space at top of chart (pixels?). Default: "30".

- chartWidth:

  googleVis chart area width. A simple number is a value in pixels; a
  string containing a number followed by `%` is a percentage. Default:
  `"60%"`

- chartHeight:

  googleVis chart area height. A simple number is a value in pixels; a
  string containing a number followed by `%` is a percentage. Default:
  `"80%"`

- axisTitlesPosition:

  Where to place the googleVis axis titles, compared to the chart area.
  Supported values: "in" - Draw the axis titles inside the the chart
  area. "out" - Draw the axis titles outside the chart area. "none" -
  Omit the axis titles.

- dataOpacity:

  The transparency of googleVis data points, with 1.0 being completely
  opaque and 0.0 fully transparent.

- options:

  a list to be passed to the googleVis function giving complete control
  over the output. Specifying a value for `options` overwrites all other
  plotting variables.

- hAxis.logScale:

  logical, whether or not to use a log scale on the horizontal axis.
  Default = TRUE.

- ylim:

  the y limits of the `which="boot"` plots.

- text:

  logical, whether or not to add text labels to classic boot plot.
  Default = `FALSE`.

- backgroundColor:

  The background colour for the main area of the chart. A simple HTML
  color string, for example: 'red' or '#00cc00'. Default: 'transparent'

- legend.position:

  the postion of the legend for classic plots. Default
  `legend.position="right"` alternatives include `legend.position="top"`
  and `legend.position="bottom"`

- jitterk:

  amount of jittering of the model size in the lvk and boot plots.
  Default = 0.1.

- srt:

  when `text=TRUE`, the angle of rotation for the text labels. Default =
  45.

- max.circle:

  determines the maximum circle size. Default = 15.

- min.prob:

  lower bound on the probability of a model being selected. If a model
  has a selection probability lower than `min.prob` it will not be
  plotted.

- ...:

  further arguments (currently unused)

## See also

[`bglmnet`](https://garthtarr.github.io/mplot/reference/bglmnet.md)
