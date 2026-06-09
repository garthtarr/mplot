# Forced Expiratory Volume

This data set consists of 654 observations on youths aged 3 to 19 from
East Boston recorded duing the middle to late 1970's. Forced expiratory
volume (FEV), a measure of lung capacity, is the variable of interest.
Age and height are two continuous predictors. Sex and smoke are two
categorical predictors.

## Usage

``` r
data(fev)
```

## Format

A data frame with 654 observations on 5 variables.

- age:

  Age (years)

- fev:

  Forced expiratory volume (liters). Roughly the amount of air an
  individual can exhale in the first second of a forceful breath.

- height:

  Height (inches).

- sex:

  Female is 0. Male is 1.

- smoke:

  A binary variable indicating whether or not the youth smokes.
  Nonsmoker is 0. Smoker is 1.

## Details

Copies of this data set can also be found in the `coneproj` and `tmle`
packages.

## References

Tager, I. B., Weiss, S. T., Rosner, B., and Speizer, F. E. (1979).
Effect of parental cigarette smoking on pulmonary function in children.
*American Journal of Epidemiology*, **110**, 15-26.

Rosner, B. (1999). *Fundamentals of Biostatistics*, 5th Ed., Pacific
Grove, CA: Duxbury.

Kahn, M.J. (2005). An Exhalent Problem for Teaching Statistics. *Journal
of Statistics Education*, **13**(2).
http://www.amstat.org/publications/jse/v13n2/datasets.kahn.html

## Examples

``` r
data(fev)
full.mod = lm(fev~.,data=fev)
step(full.mod)
#> Start:  AIC=-1154.18
#> fev ~ age + height + sex + smoke
#> 
#>          Df Sum of Sq    RSS      AIC
#> <none>                110.28 -1154.18
#> - smoke   1     0.368 110.65 -1154.00
#> - sex     1     3.803 114.08 -1134.00
#> - age     1     8.099 118.38 -1109.83
#> - height  1    81.505 191.78  -794.28
#> 
#> Call:
#> lm(formula = fev ~ age + height + sex + smoke, data = fev)
#> 
#> Coefficients:
#> (Intercept)          age       height          sex        smoke  
#>    -4.45697      0.06551      0.10420      0.15710     -0.08725  
#> 
```
