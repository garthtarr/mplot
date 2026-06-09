# Print method for an af object

Prints basic output of the bootstrap results of an af object.

## Usage

``` r
# S3 method for class 'af'
print(x, best.only = TRUE, ...)
```

## Arguments

- x:

  an `af` object, the result of
  [`af`](https://garthtarr.github.io/mplot/reference/af.md)

- best.only:

  logical determining whether the output used the standard fence
  approach of only considering the best models that pass the fence
  (`TRUE`) or if it should take into account all models that pass the
  fence at each boundary value (`FALSE`).

- ...:

  further arguments (currently unused)
