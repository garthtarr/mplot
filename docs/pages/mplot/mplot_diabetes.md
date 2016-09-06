---
title: Diabetes example
tags:
  - example
  - linear models
keywords: "example, AF, VIP, MSP, linear model"
last_updated: "August 31, 2016"
summary: "Diabetes example."
series: "Examples"
weight: 10
sidebar: mplot_sidebar
permalink: mplot_diabetes.html
folder: mplot
---


The [table below](#tab:diabetes) shows a subset of the diabetes data used in [Tibshirani, Johnstone, Hastie and Efron (2004)](http://doi.org/10.1214/009053604000000067 "Tibshirani RJ, Johnstone I, Hastie T, Efron B (2004). “Least Angle Regression.” The Annals of Statistics, 32(2), 407–499.").  There are 10 explanatory variables, including age (`age`), sex (`sex`), body mass index (`bmi`) and mean arterial blood pressure (`map`) of 442 patients as well as six blood serum measurements (`tc`, `ldl`, `hdl`, `tch`, `ltg` and `glu`).  The response (`y`) is a continuous measure of disease progression one year after the baseline measurements. 

<div id="tab:diabetes">
<table class="tg">
  <tr>
    <th class="tg-yw4l"></th>
    <th class="tg-yw4l"></th>
    <th class="tg-yw4l"></th>
    <th class="tg-yw4l"></th>
    <th class="tg-yw4l"></th>
    <th class="tg-yw4l" colspan="6">Serum measurements</th>
    <th class="tg-yw4l">Response</th>
  </tr>
  <tr>
    <td class="tg-yw4l">Patient</td>
    <td class="tg-yw4l">age</td>
    <td class="tg-yw4l">sex</td>
    <td class="tg-yw4l">bmi</td>
    <td class="tg-yw4l">map</td>
    <td class="tg-yw4l">tc</td>
    <td class="tg-yw4l">ldl</td>
    <td class="tg-yw4l">hdl</td>
    <td class="tg-yw4l">tch</td>
    <td class="tg-yw4l">ltg</td>
    <td class="tg-yw4l">glu</td>
    <td class="tg-yw4l">y</td>
  </tr>
  <tr>
    <td class="tg-yw4l">1</td>
    <td class="tg-yw4l">59</td>
    <td class="tg-yw4l">2</td>
    <td class="tg-yw4l">32.1</td>
    <td class="tg-yw4l">101</td>
    <td class="tg-yw4l">157</td>
    <td class="tg-yw4l">93.2</td>
    <td class="tg-yw4l">38</td>
    <td class="tg-yw4l">4</td>
    <td class="tg-yw4l">4.9</td>
    <td class="tg-yw4l">87</td>
    <td class="tg-yw4l">151</td>
  </tr>
  <tr>
    <td class="tg-yw4l">2</td>
    <td class="tg-yw4l">48</td>
    <td class="tg-yw4l">1</td>
    <td class="tg-yw4l">21.6</td>
    <td class="tg-yw4l">87</td>
    <td class="tg-yw4l">183</td>
    <td class="tg-yw4l">103.2</td>
    <td class="tg-yw4l">70</td>
    <td class="tg-yw4l">3</td>
    <td class="tg-yw4l">3.9</td>
    <td class="tg-yw4l">69</td>
    <td class="tg-yw4l">75</td>
  </tr>
  <tr>
    <td class="tg-yw4l">3</td>
    <td class="tg-yw4l">72</td>
    <td class="tg-yw4l">2</td>
    <td class="tg-yw4l">30.5</td>
    <td class="tg-yw4l">93</td>
    <td class="tg-yw4l">156</td>
    <td class="tg-yw4l">93.6</td>
    <td class="tg-yw4l">41</td>
    <td class="tg-yw4l">4</td>
    <td class="tg-yw4l">4.7</td>
    <td class="tg-yw4l">85</td>
    <td class="tg-yw4l">141</td>
  </tr>
  <tr>
    <td class="tg-yw4l">...</td>
    <td class="tg-yw4l">...</td>
    <td class="tg-yw4l">...</td>
    <td class="tg-yw4l">...</td>
    <td class="tg-yw4l">...</td>
    <td class="tg-yw4l">...</td>
    <td class="tg-yw4l">...</td>
    <td class="tg-yw4l">...</td>
    <td class="tg-yw4l">...</td>
    <td class="tg-yw4l">...</td>
    <td class="tg-yw4l">...</td>
    <td class="tg-yw4l">...</td>
  </tr>
  <tr>
    <td class="tg-yw4l">441</td>
    <td class="tg-yw4l">36</td>
    <td class="tg-yw4l">1</td>
    <td class="tg-yw4l">30.0</td>
    <td class="tg-yw4l">95</td>
    <td class="tg-yw4l">201</td>
    <td class="tg-yw4l">125.2</td>
    <td class="tg-yw4l">42</td>
    <td class="tg-yw4l">5</td>
    <td class="tg-yw4l">5.1</td>
    <td class="tg-yw4l">85</td>
    <td class="tg-yw4l">220</td>
  </tr>
  <tr>
    <td class="tg-yw4l">442</td>
    <td class="tg-yw4l">36</td>
    <td class="tg-yw4l">1</td>
    <td class="tg-yw4l">19.6</td>
    <td class="tg-yw4l">71</td>
    <td class="tg-yw4l">250</td>
    <td class="tg-yw4l">133.2</td>
    <td class="tg-yw4l">97</td>
    <td class="tg-yw4l">3</td>
    <td class="tg-yw4l">4.6</td>
    <td class="tg-yw4l">92</td>
    <td class="tg-yw4l">57</td>
  </tr>
</table>
</div>
_Table: measurements on 442 diabetes patients over 10 potential predictor variables and the response variable, a measure of disease progression after one year._

The [figure below](#fig:diabetesmain) shows the results of the main methods for the diabetes data obtained using the following code.

```s
lm.d = lm(y ~ ., data = diabetes)
vis.d = vis(lm.d, B = 200)
af.d = af(lm.d, B = 200, n.c = 100, c.max = 100)
plot(vis.d, interactive = FALSE, which = "vip")
plot(vis.d, interactive = FALSE, which = "boot", max.circle = 0.25, highlight = "hdl")
plot(af.d, interactive = FALSE, best.only = TRUE, legend.position = "bottomright")
plot(af.d, interactive = FALSE, best.only = FALSE)
```


A striking feature of the variable inclusion plot is the non-monotonic nature of the `hdl` line.   As the penalty value increases, and a more parsimonious model is sought, the `hdl` variable is selected more frequently while at the same time other variables with similar information are dropped.  Such paths occur when a group of variables contains similar information to another variable.  The `hdl` line is a less extreme example of what occurs with $x_8$ in the artificial example (see Figure \ref{plot.vis}).   The path for the age variable lies below the path for the redundant variable, indicating that it does not provide any useful information. The `bmi` and `ltg` paths are horizontal with a bootstrap probability of 1 for all penalty values indicating that they are very important variables, as are `map` and `sex`.  From the variable inclusion plot alone, it is not obvious whether `tc` or `hdl` is the next most important variable.  Some guidance on this issue is provided by the model stability and adaptive fence plots.

In order to determine which circles correspond to which models in the static version of the bootstrap stability plot, we need to consult the print output of the `vis` object.

```s
vis.d
```

```
                     name prob logLikelihood
                      y~1 1.00      -2547.17
                    y~bmi 0.73      -2454.02
                y~bmi+ltg 1.00      -2411.20
            y~bmi+map+ltg 0.69      -2402.61
         y~bmi+map+tc+ltg 0.42      -2397.48
        y~bmi+map+hdl+ltg 0.32      -2397.71
    y~sex+bmi+map+hdl+ltg 0.67      -2390.13
 y~sex+bmi+map+tc+ldl+ltg 0.47      -2387.30
```



As in the variable inclusion plots, it is clear that the two most important variables are `bmi` and `ltg`, and the third most important variable is  `map`.  In models of size four (including the intercept), the model with `bmi`, `ltg` and `map` was selected in 69% of bootstrap resamples.  There is no clear dominant model in models of size five, with `tc` and `hdl` both competing to be included.  In models of size six, the combination of `sex` and `hdl` with the core variables `bmi`, `map` and `ltg`, is the most stable option; it is selected in 67% of bootstrap resamples.  As the size of the model space in dimension six is much larger than the size of the model space for dimension four, it could be suggested that the 0.67 empirical probability for the {`bmi`, `map`, `ltg`, `sex`, `hdl`} model is a stronger result than the 0.69 result for the {`bmi`, `ltg`, `map`} model. 

The adaptive fence plots in the bottom row of the [figure below](#fig:diabetesmain) show a clear peak for the model with just `bmi` and `ltg`. There are two larger models that also occupy regions of stability, albeit with much lower peaks.  These are {`bmi`, `map`, `ltg`} and {`bmi`, `map`, `ltg`, `sex`, `hdl`} which also showed up as dominant models in the model stability plots.  Contrasting  `best.only = TRUE` in the lower left panel with  `best.only = FALSE` in the lower right panel, we can see that the peaks tend to be more clearly distinguished, though the regions of stability remain largely unchanged.

Stepwise approaches using a forward search or backward search with the AIC or BIC all yield a model with {`bmi`, `map`, `ltg`, `sex`, `ldl`, `tc`}.  This model was selected 47% of the time in models of size 7.  The agreement between the stepwise methods may be comforting for the researcher, but it does not aid a discussion about what other models may be worth exploring.

An interactive version of the plots in the [figure below](#fig:diabetesmain) is available at garthtarr.com/apps/mplot.

<div id="fig:diabetesmain">
<img src="images/dbmain.png">

</div>
_Figure: diabetes main effects example._







To incorporate interaction terms, we suggest selecting the main effects first, then regressing the relevant interaction terms on the residuals from the main effects model.  This approach ensures that the main effects are always taken into account. In this example, we estimate the dominant model of dimension six and obtain the fitted residuals.  The interaction terms are then regressed on the fitted residuals. 

```s
lm.d.main = lm(y ~ sex + bmi + map + hdl + ltg, data = diabetes)
summary(lm.d.main)
db.main = diabetes[, c("sex", "bmi", "map", "hdl", "ltg")]
db.main$y = lm.d.main$residuals
lm.d.int = lm(y ~ .*. - sex - bmi - map - hdl - ltg, data = db.main)
vis.d.int = vis(lm.d.int, B = 200)
af.d.int = af(lm.d.int, B = 200, n.c = 100)
vis.d.int
```

```
                              name prob logLikelihood
                               y~1 1.00      -2390.13
 y~bmi.map+bmi.hdl+map.ltg+hdl.ltg 0.56      -2385.89
```

The result can be found in the [figure below](#fig:diabetesint) The variable inclusion plots suggest that the most important interaction terms are `hdl.ltg`, `bmi.hdl`, `map.ltg` and `bmi.map`.  The model stability plot suggests that there are no dominant models of size 2, 3 or 4.  Furthermore there are no models of size 2, 3 or 4 that make large improvements in description loss. There is a dominant model of dimension 5 that is selected in 56% of bootstrap resamples.  The variables selected in the dominant model are {`bmi.map`, `bmi.hdl`, `map.ltg`, `hdl.ltg`}, which can be found in the print output above.  Furthermore, this model does make a reasonable improvement in description loss, almost in line with the full model.  This finding is reinforced in the adaptive fence plots where there are only two regions of stability, one for the null model and another for the {`bmi.map`, `bmi.hdl`, `map.ltg`, `hdl.ltg`} model. In this instance, the difference between `best.only = TRUE` and `best.only = FALSE` is minor.

<div id="fig:diabetesint">
<img src="images/dbint.png">

</div>
_Figure: diabetes interactions terms example._



Hence, as a final model for the diabetes example we suggest including the main effects {`bmi`, `map`, `ltg`, `sex`, `hdl`} and the interaction effects {`bmi.map`, `bmi.hdl`, `map.ltg`, `hdl.ltg`}.  Further investigation can also be useful.  For example, we could use cross validation to compare the model with interaction effects, the model with just main effects and other simpler models that were identified as having peaks in the adaptive fence.  Researchers should also incorporate their specialist knowledge of the predictors and evaluate whether or not the estimated model is sensible from a scientific perspective.


