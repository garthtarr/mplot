# Print method for a vis object

Prints basic output of the bootstrap results of an vis object.

## Usage

``` r
# S3 method for class 'vis'
print(x, min.prob = 0.3, print.full.model = FALSE, ...)
```

## Arguments

- x:

  a `vis` object, the result of
  [`vis`](https://garthtarr.github.io/mplot/reference/vis.md)

- min.prob:

  a lower bound on the probability of selection before the result is
  printed

- print.full.model:

  logical, determines if the full model gets printed or not.
  Default=`FALSE`.

- ...:

  further arguments (currently unused)
