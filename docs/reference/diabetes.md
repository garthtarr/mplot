# Blood and other measurements in diabetics

The diabetes data frame has 442 rows and 11 columns. These are the data
used in Efron et al. (2004).

## Usage

``` r
data(diabetes)
```

## Format

A data frame with 442 observations on 11 variables.

- age:

  Age

- sex:

  Gender

- bmi:

  Body mass index

- map:

  Mean arterial pressure (average blood pressure)

- tc:

  Total cholesterol (mg/dL)? Desirable range: below 200 mg/dL

- ldl:

  Low-density lipoprotein ("bad" cholesterol)? Desirable range: below
  130 mg/dL

- hdl:

  High-density lipoprotein ("good" cholesterol)? Desirable range: above
  40 mg/dL

- tch:

  Blood serum measurement

- ltg:

  Blood serum measurement

- glu:

  Blood serum measurement (glucose?)

- y:

  A quantitative measure of disease progression one year after baseline

## Details

Data sourced from http://web.stanford.edu/~hastie/Papers/LARS

## References

Efron, B., Hastie, T., Johnstone, I., Tibshirani, R., (2004). Least
angle regression. The Annals of Statistics 32(2) 407-499. DOI:
10.1214/009053604000000067

## Examples

``` r
data(diabetes)
full.mod = lm(y~.,data=diabetes)
```
