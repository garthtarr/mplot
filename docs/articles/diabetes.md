# Diabetes example

The [table below](#tab:diabetes) shows a subset of the diabetes data
used in Tibshirani et al. ([2004](#ref-Tibshirani:2004)). There are 10
explanatory variables, including age (`age`), sex (`sex`), body mass
index (`bmi`) and mean arterial blood pressure (`map`) of 442 patients
as well as six blood serum measurements (`tc`, `ldl`, `hdl`, `tch`,
`ltg` and `glu`). The response (`y`) is a continuous measure of disease
progression one year after the baseline measurements.

``` r

library(mplot)
knitr::kable(head(diabetes))
```

| age | sex |  bmi | map |  tc |   ldl | hdl | tch |    ltg | glu |   y |
|----:|----:|-----:|----:|----:|------:|----:|----:|-------:|----:|----:|
|  59 |   2 | 32.1 | 101 | 157 |  93.2 |  38 |   4 | 4.8598 |  87 | 151 |
|  48 |   1 | 21.6 |  87 | 183 | 103.2 |  70 |   3 | 3.8918 |  69 |  75 |
|  72 |   2 | 30.5 |  93 | 156 |  93.6 |  41 |   4 | 4.6728 |  85 | 141 |
|  24 |   1 | 25.3 |  84 | 198 | 131.4 |  40 |   5 | 4.8903 |  89 | 206 |
|  50 |   1 | 23.0 | 101 | 192 | 125.4 |  52 |   4 | 4.2905 |  80 | 135 |
|  23 |   1 | 22.6 |  89 | 139 |  64.8 |  61 |   2 | 4.1897 |  68 |  97 |

*Table: measurements on 442 diabetes patients over 10 potential
predictor variables and the response variable, a measure of disease
progression after one year.*

The [figure below](#fig:diabetesmain) shows the results of the main
methods for the diabetes data obtained using the following code.

``` r

lm.d = lm(y ~ ., data = diabetes)
summary(lm.d)
```

    ## 
    ## Call:
    ## lm(formula = y ~ ., data = diabetes)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -155.827  -38.536   -0.228   37.806  151.353 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -334.56714   67.45462  -4.960 1.02e-06 ***
    ## age           -0.03636    0.21704  -0.168 0.867031    
    ## sex          -22.85965    5.83582  -3.917 0.000104 ***
    ## bmi            5.60296    0.71711   7.813 4.30e-14 ***
    ## map            1.11681    0.22524   4.958 1.02e-06 ***
    ## tc            -1.09000    0.57333  -1.901 0.057948 .  
    ## ldl            0.74645    0.53083   1.406 0.160390    
    ## hdl            0.37200    0.78246   0.475 0.634723    
    ## tch            6.53383    5.95864   1.097 0.273459    
    ## ltg           68.48312   15.66972   4.370 1.56e-05 ***
    ## glu            0.28012    0.27331   1.025 0.305990    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 54.15 on 431 degrees of freedom
    ## Multiple R-squared:  0.5177, Adjusted R-squared:  0.5066 
    ## F-statistic: 46.27 on 10 and 431 DF,  p-value: < 2.2e-16

As the [`vis()`](https://garthtarr.github.io/mplot/reference/vis.md) and
[`af()`](https://garthtarr.github.io/mplot/reference/af.md) functions
can take a while to run, especially with such large values of `B` and
`n.c`, we run them once, save the results which can then be reloaded
later without having to rerun the functions.

``` r

vis.d = vis(lm.d, B = 200)
af.d = af(lm.d, B = 200, n.c = 100, c.max = 100)
save(lm.d, vis.d, af.d, file = "diabetes_main.RData")
```

In the next code chunk, we load in the saved objects and visualise them.

``` r

load("diabetes_main.RData")
```

``` r

plot(vis.d, which = "vip", interactive = TRUE, tag = "chart")
```

``` r

plot(vis.d, which = "boot", max.circle = 0.25, highlight = "hdl", interactive = TRUE, tag = "chart", seed = 1)
```

``` r

plot(af.d, best.only = TRUE, legend.position = "bottomright", interactive = TRUE, tag = "chart")
```

``` r

plot(af.d, best.only = FALSE, interactive = TRUE, tag = "chart")
```

*Figure: diabetes main effects example.*

A striking feature of the variable inclusion plot is the non-monotonic
nature of the `hdl` line. As the penalty value increases, and a more
parsimonious model is sought, the `hdl` variable is selected more
frequently while at the same time other variables with similar
information are dropped. Such paths occur when a group of variables
contains similar information to another variable. The `hdl` line is a
less extreme example of what occurs with \\x_8\\ in the artificial
example (see
[here](https://garthtarr.github.io/mplot/articles/vip#fig:plotvis)). The
path for the age variable lies below the path for the redundant
variable, indicating that it does not provide any useful information.
The `bmi` and `ltg` paths are horizontal with a bootstrap probability of
1 for all penalty values indicating that they are very important
variables, as are `map` and `sex`. From the variable inclusion plot
alone, it is not obvious whether `tc` or `hdl` is the next most
important variable. Some guidance on this issue is provided by the model
stability and adaptive fence plots.

In order to determine which circles correspond to which models in the
static version of the bootstrap stability plot, we need to consult the
print output of the `vis` object.

``` r

vis.d
```

    ##                      name prob logLikelihood
    ##                       y~1 1.00      -2547.17
    ##                     y~bmi 0.76      -2454.02
    ##                 y~bmi+ltg 1.00      -2411.20
    ##             y~bmi+map+ltg 0.70      -2402.61
    ##         y~bmi+map+hdl+ltg 0.38      -2397.71
    ##          y~bmi+map+tc+ltg 0.33      -2397.48
    ##     y~sex+bmi+map+hdl+ltg 0.66      -2390.13
    ##  y~sex+bmi+map+tc+ldl+ltg 0.44      -2387.30

As in the variable inclusion plots, it is clear that the two most
important variables are `bmi` and `ltg`, and the third most important
variable is `map`. In models of size four (including the intercept), the
model with `bmi`, `ltg` and `map` was selected in 69% of bootstrap
resamples. There is no clear dominant model in models of size five, with
`tc` and `hdl` both competing to be included. In models of size six, the
combination of `sex` and `hdl` with the core variables `bmi`, `map` and
`ltg`, is the most stable option; it is selected in 67% of bootstrap
resamples. As the size of the model space in dimension six is much
larger than the size of the model space for dimension four, it could be
suggested that the 0.67 empirical probability for the {`bmi`, `map`,
`ltg`, `sex`, `hdl`} model is a stronger result than the 0.69 result for
the {`bmi`, `ltg`, `map`} model.

The adaptive fence plots in the bottom row of the [figure
below](#fig:diabetesmain) show a clear peak for the model with just
`bmi` and `ltg`. There are two larger models that also occupy regions of
stability, albeit with much lower peaks. These are {`bmi`, `map`, `ltg`}
and {`bmi`, `map`, `ltg`, `sex`, `hdl`} which also showed up as dominant
models in the model stability plots. Contrasting `best.only = TRUE` in
the lower left panel with `best.only = FALSE` in the lower right panel,
we can see that the peaks tend to be more clearly distinguished, though
the regions of stability remain largely unchanged.

Stepwise approaches using a forward search or backward search with the
AIC or BIC all yield a model with {`bmi`, `map`, `ltg`, `sex`, `ldl`,
`tc`}. This model was selected 47% of the time in models of size 7. The
agreement between the stepwise methods may be comforting for the
researcher, but it does not aid a discussion about what other models may
be worth exploring.

Interactive versions of the plots in the [figure
below](#fig:diabetesmain) are shown above and a Shiny app example
resulting from a call to `mplot(lm.d, vis.d, af.d)` is available at
garthtarr.com/apps/mplot.

To incorporate interaction terms, we suggest selecting the main effects
first, then regressing the relevant interaction terms on the residuals
from the main effects model. This approach ensures that the main effects
are always taken into account. In this example, we estimate the dominant
model of dimension six and obtain the fitted residuals. The interaction
terms are then regressed on the fitted residuals.

``` r

lm.d.main = lm(y ~ sex + bmi + map + hdl + ltg, data = diabetes)
summary(lm.d.main)
```

    ## 
    ## Call:
    ## lm(formula = y ~ sex + bmi + map + hdl + ltg, data = diabetes)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -150.031  -39.317   -0.561   37.307  148.382 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -217.6849    35.7638  -6.087 2.53e-09 ***
    ## sex          -22.4742     5.7640  -3.899 0.000112 ***
    ## bmi            5.6431     0.7037   8.019 9.93e-15 ***
    ## map            1.1232     0.2172   5.171 3.55e-07 ***
    ## hdl           -1.0644     0.2417  -4.404 1.34e-05 ***
    ## ltg           43.2344     5.9874   7.221 2.32e-12 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 54.35 on 436 degrees of freedom
    ## Multiple R-squared:  0.5086, Adjusted R-squared:  0.503 
    ## F-statistic: 90.26 on 5 and 436 DF,  p-value: < 2.2e-16

``` r

db.main = diabetes[, c("sex", "bmi", "map", "hdl", "ltg")]
db.main$y = lm.d.main$residuals
lm.d.int = lm(y ~ .*. - sex - bmi - map - hdl - ltg, data = db.main)
summary(lm.d.int)
```

    ## 
    ## Call:
    ## lm(formula = y ~ . * . - sex - bmi - map - hdl - ltg, data = db.main)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -150.898  -39.805   -1.202   37.686  159.261 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)  3.9403663 18.3663274   0.215   0.8302   
    ## sex:bmi      0.1762242  1.4482768   0.122   0.9032   
    ## sex:map      0.1443193  0.4077458   0.354   0.7236   
    ## sex:hdl      0.0692292  0.3943839   0.176   0.8607   
    ## sex:ltg     -4.7786900  8.8413893  -0.540   0.5891   
    ## bmi:map      0.0553744  0.0336520   1.646   0.1006   
    ## bmi:hdl     -0.1186993  0.0488557  -2.430   0.0155 * 
    ## bmi:ltg     -0.0002419  0.5619078   0.000   0.9997   
    ## map:hdl     -0.0048466  0.0117988  -0.411   0.6814   
    ## map:ltg     -0.2961337  0.1985835  -1.491   0.1366   
    ## hdl:ltg      0.7190590  0.2767715   2.598   0.0097 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 54.11 on 431 degrees of freedom
    ## Multiple R-squared:  0.02006,    Adjusted R-squared:  -0.002681 
    ## F-statistic: 0.8821 on 10 and 431 DF,  p-value: 0.55

``` r

vis.d.int = vis(lm.d.int, B = 200)
af.d.int = af(lm.d.int, B = 200, n.c = 100)
save(lm.d.int, vis.d.int, af.d.int, file = "diabetes_int.RData")
```

``` r

load("diabetes_int.RData")
vis.d.int
```

    ##                               name prob logLikelihood
    ##                                y~1  1.0      -2390.13
    ##  y~bmi.map+bmi.hdl+map.ltg+hdl.ltg  0.5      -2385.89

                                  name prob logLikelihood
                                   y~1 1.00      -2390.13
     y~bmi.map+bmi.hdl+map.ltg+hdl.ltg 0.56      -2385.89

The result can be found in the [figure below](#fig:diabetesint). The
variable inclusion plots suggest that the most important interaction
terms are `hdl.ltg`, `bmi.hdl`, `map.ltg` and `bmi.map`. The model
stability plot suggests that there are no dominant models of size 2, 3
or 4. Furthermore there are no models of size 2, 3 or 4 that make large
improvements in description loss. There is a dominant model of dimension
5 that is selected in 56% of bootstrap resamples. The variables selected
in the dominant model are {`bmi.map`, `bmi.hdl`, `map.ltg`, `hdl.ltg`},
which can be found in the print output above. Furthermore, this model
does make a reasonable improvement in description loss, almost in line
with the full model. This finding is reinforced in the adaptive fence
plots where there are only two regions of stability, one for the null
model and another for the {`bmi.map`, `bmi.hdl`, `map.ltg`, `hdl.ltg`}
model. In this instance, the difference between `best.only = TRUE` and
`best.only = FALSE` is minor.

``` r

plot(vis.d.int, which = "vip", interactive = TRUE, tag = "chart")
```

``` r

plot(vis.d.int, which = "boot", max.circle = 0.25, highlight = "bmi.map", interactive = TRUE, tag = "chart", seed = 1)
```

``` r

plot(af.d.int, best.only = TRUE, legend.position = "bottomright", interactive = TRUE, tag = "chart")
```

Hence, as a final model for the diabetes example we suggest including
the main effects {`bmi`, `map`, `ltg`, `sex`, `hdl`} and the interaction
effects {`bmi.map`, `bmi.hdl`, `map.ltg`, `hdl.ltg`}. Further
investigation can also be useful. For example, we could use cross
validation to compare the model with interaction effects, the model with
just main effects and other simpler models that were identified as
having peaks in the adaptive fence. Researchers should also incorporate
their specialist knowledge of the predictors and evaluate whether or not
the estimated model is sensible from a scientific perspective.

#### References

Tibshirani, R. J., Johnstone, I., Hastie, T., & Efron, B. (2004). Least
angle regression. *The Annals of Statistics*, 32(2), 407–499.
DOI:[10.1214/009053604000000067](https://doi.org/10.1214/009053604000000067)
