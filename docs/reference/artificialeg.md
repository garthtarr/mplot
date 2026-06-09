# Artificial example

An artificial data set which causes stepwise regression procedures to
select a non-parsimonious model. The true model is a simple linear
regression of y against x8.

## Usage

``` r
data(artificialeg)
```

## Format

A data frame with 50 observations on 10 variables.

## Details

Inspired by the pathoeg data set in the MPV pacakge.

## Examples

``` r
data(artificialeg)
full.mod = lm(y~.,data=artificialeg)
step(full.mod)
#> Start:  AIC=79.3
#> y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9
#> 
#>        Df Sum of Sq    RSS    AIC
#> - x8    1    0.2423 163.94 77.374
#> - x3    1    0.6946 164.39 77.512
#> - x2    1    0.7107 164.41 77.517
#> - x6    1    1.3051 165.00 77.698
#> - x5    1    1.4425 165.14 77.739
#> - x9    1    1.6065 165.31 77.789
#> - x7    1    1.8835 165.58 77.873
#> - x1    1    3.4999 167.20 78.358
#> - x4    1    5.7367 169.44 79.023
#> <none>              163.70 79.301
#> 
#> Step:  AIC=77.37
#> y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x9
#> 
#>        Df Sum of Sq    RSS     AIC
#> <none>              163.94  77.374
#> - x2    1    20.359 184.30  81.227
#> - x5    1    25.966 189.91  82.726
#> - x9    1    33.607 197.55  84.698
#> - x4    1    34.504 198.45  84.925
#> - x7    1    62.097 226.04  91.434
#> - x1    1    68.253 232.19  92.778
#> - x3    1    71.301 235.24  93.430
#> - x6    1   107.873 271.81 100.655
#> 
#> Call:
#> lm(formula = y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x9, data = artificialeg)
#> 
#> Coefficients:
#> (Intercept)           x1           x2           x3           x4           x5  
#>     -0.1143       0.8019       0.4011      -0.8083      -0.3514       0.4927  
#>          x6           x7           x9  
#>     -0.7738      -0.5772       0.5478  
#> 
# generating model
n=50
set.seed(8) # a seed of 2 also works
x1 = rnorm(n,0.22,2)
x7 = 0.5*x1 + rnorm(n,0,sd=2)
x6 = -0.75*x1 + rnorm(n,0,3)
x3 = -0.5-0.5*x6 + rnorm(n,0,2)
x9 = rnorm(n,0.6,3.5)
x4 = 0.5*x9 + rnorm(n,0,sd=3)
x2 = -0.5 + 0.5*x9 + rnorm(n,0,sd=2)
x5 = -0.5*x2+0.5*x3+0.5*x6-0.5*x9+rnorm(n,0,1.5)
x8 = x1 + x2 -2*x3 - 0.3*x4 + x5 - 1.6*x6 - 1*x7 + x9 +rnorm(n,0,0.5)
y = 0.6*x8 + rnorm(n,0,2)
artificialeg = round(data.frame(x1,x2,x3,x4,x5,x6,x7,x8,x9,y),1)
```
