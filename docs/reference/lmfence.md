# The fence procedure for linear models

This function implements the fence procedure to find the best linear
model.

## Usage

``` r
lmfence(mf, cstar, nvmax, adaptive = TRUE, trace = TRUE, force.in = NULL, ...)
```

## Arguments

- mf:

  an object of class [`lm`](https://rdrr.io/r/stats/lm.html) specifying
  the full model.

- cstar:

  the boundary of the fence, typically found through bootstrapping.

- nvmax:

  the maximum number of variables that will be be considered in the
  model.

- adaptive:

  logical. If `TRUE` the boundary of the fence is given by cstar.
  Otherwise, it the original (non-adaptive) fence is performed where the
  boundary is \\c^\* \hat{\sigma}\_{M,\tilde{M}}\\.

- trace:

  logical. If `TRUE` the function prints out its progress as it iterates
  up through the dimensions.

- force.in:

  the names of variables that should be forced into all estimated
  models.

- ...:

  further arguments (currently unused)

## References

Jiming Jiang, Thuan Nguyen, J. Sunil Rao, A simplified adaptive fence
procedure, Statistics & Probability Letters, Volume 79, Issue 5, 1 March
2009, Pages 625-629, http://dx.doi.org/10.1016/j.spl.2008.10.014.

## See also

[`af`](https://garthtarr.github.io/mplot/reference/af.md),
[`glmfence`](https://garthtarr.github.io/mplot/reference/glmfence.md)

Other fence:
[`af()`](https://garthtarr.github.io/mplot/reference/af.md),
[`glmfence()`](https://garthtarr.github.io/mplot/reference/glmfence.md)

## Examples

``` r
n = 40 # sample size
beta = c(1,2,3,0,0)
K=length(beta)
set.seed(198)
X = cbind(1,matrix(rnorm(n*(K-1)),ncol=K-1))
e = rnorm(n)
y = X%*%beta + e
dat = data.frame(y,X[,-1])
# Non-adaptive approach (not recommended)
lm1 = lm(y~.,data=dat)
lmfence(lm1,cstar=log(n),adaptive=FALSE)
#> Null model (Not a candidate model) 
#> Model size: 2 (No candidate models found) 
#> Model size: 3 
#>  Candidate model found via leaps. 
#> Exploring other options at this model size. 
#> hatQm: 55.56 ; Upper bound: 59.52 
#> hatQm <= UB: TRUE 
#> y ~ X1 + X2
#> [[1]]
#> y ~ X1 + X2
#> 
```
