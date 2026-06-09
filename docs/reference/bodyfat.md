# Body fat data set

A data frame with 128 observations on 15 variables.

## Usage

``` r
data(bodyfat)
```

## Format

A data frame with 128 observations on 15 variables.

- Id:

  Identifier

- Bodyfat:

  Bodyfat percentage

- Age:

  Age (years)

- Weight:

  Weight (kg)

- Height:

  Height (inches)

- Neck:

  Neck circumference (cm)

- Chest:

  Chest circumference (cm)

- Abdo:

  Abdomen circumference (cm) "at the umbilicus and level with the iliac
  crest"

- Hip:

  Hip circumference (cm)

- Thigh:

  Thigh circumference (cm)

- Knee:

  Knee circumference (cm)

- Ankle:

  Ankle circumference (cm)

- Bic:

  Extended biceps circumference (cm)

- Fore:

  Forearm circumference (cm)

- Wrist:

  Wrist circumference (cm) "distal to the styloid processes"

## Details

A subset of the 252 observations available in the `mfp` package. The
selected observations avoid known high leverage points and outliers. The
unused points from the data set could be used to validate selected
models.

## References

Johnson W (1996, Vol 4). Fitting percentage of body fat to simple body
measurements. Journal of Statistics Education. Bodyfat data retrieved
from http://www.amstat.org/publications/jse/v4n1/datasets.johnson.html
An expanded version is included in the `mfp` R package.

## Examples

``` r
data(bodyfat)
full.mod = lm(Bodyfat~.,data=subset(bodyfat,select=-Id))
```
