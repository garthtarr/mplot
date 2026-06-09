# Model selection and stability curves

Opens a shiny GUI to investigate a range of model selection and
stability issues

## Usage

``` r
mplot(mf, ...)
```

## Arguments

- mf:

  a fitted model.

- ...:

  objects of type `vis` or `af` or `bglmnet`.

## References

Tarr G, Mueller S and Welsh AH (2018). mplot: An R Package for Graphical
Model Stability and Variable Selection Procedures. Journal of
Statistical Software, 83(9), pp. 1-28. doi: 10.18637/jss.v083.i09

## Examples

``` r
n = 100
set.seed(11)
e = rnorm(n)
x1 = rnorm(n)
x2 = rnorm(n)
x3 = x1^2
x4 = x2^2
x5 = x1*x2
y = 1 + x1 + x2 + e
dat = round(data.frame(y,x1,x2,x3,x4,x5),2)
lm1 = lm(y ~ ., data = dat)
if (FALSE) { # \dontrun{
v1 = vis(lm1)
af1 = af(lm1)
bg1 = bglmnet(lm1)
mplot(lm1, v1, af1, bg1)
} # }
```
