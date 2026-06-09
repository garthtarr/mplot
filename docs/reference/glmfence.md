# The fence procedure for generalised linear models

This function implements the fence procedure to find the best
generalised linear model.

## Usage

``` r
glmfence(mf, cstar, nvmax, adaptive = TRUE, trace = TRUE, ...)
```

## Arguments

- mf:

  an object of class [`glm`](https://rdrr.io/r/stats/glm.html)
  specifying the full model.

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

- ...:

  further arguments (currently unused)

## References

Jiming Jiang, Thuan Nguyen, J. Sunil Rao, A simplified adaptive fence
procedure, Statistics & Probability Letters, Volume 79, Issue 5, 1 March
2009, Pages 625-629, http://dx.doi.org/10.1016/j.spl.2008.10.014.

## See also

[`af`](https://garthtarr.github.io/mplot/reference/af.md),
[`lmfence`](https://garthtarr.github.io/mplot/reference/lmfence.md)

Other fence:
[`af()`](https://garthtarr.github.io/mplot/reference/af.md),
[`lmfence()`](https://garthtarr.github.io/mplot/reference/lmfence.md)
