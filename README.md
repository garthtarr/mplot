# mplot: graphical model stability and variable selection procedures

[![Travis-CI Build Status](https://travis-ci.org/garthtarr/mplot.svg?branch=master)](https://travis-ci.org/garthtarr/mplot) [![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/mplot)](https://cran.r-project.org/package=mplot) [![](http://cranlogs.r-pkg.org/badges/mplot)](https://cran.r-project.org/package=mplot) [![DL_Total](http://cranlogs.r-pkg.org/badges/grand-total/mplot?color=blue)](https://cran.r-project.org/package=mplot)

The `mplot` package provides a collection of functions designed for exploratory model selection.

We implement model stability and variable importance plots ([Mueller and Welsh (2010)](https://doi.org/10.1111/j.1751-5823.2010.00108.x); [Murray, Heritier and Mueller (2013)](https://doi.org/10.1002/sim.5855)) as well as the adaptive fence ([Jiang et al. (2008)](https://doi.org/10.1214/07-AOS517); [Jiang et al. (2009)](https://doi.org/10.1016/j.spl.2008.10.014)) for linear and generalised linear models.   We address many practical implementation issues with sensible defaults and interactive graphics to highlight model selection stability.  The speed of implementation comes from the leaps package and multicore support for bootstrapping.

The `mplot` currently only supports linear and generalised linear models, however work is progressing to incorporate survival models and mixed models.

You can see an example of the output [here](https://garthtarr.com/apps/mplot/).

## Installation

Check that you're running the most recent versions of your currently installed R packages:

```s
update.packages()
```

### Stable release on CRAN

The mplot package has been on [CRAN](https://cran.r-project.org/package=mplot) since June 2015.  You can install it from CRAN in the usual way:

```s
install.packages("mplot")
library("mplot")
```

### Development version on Github

You can use the **devtools** package to install the development version of **mplot** from [GitHub](https://github.com/garthtarr/mplot):

```s
# install.packages("devtools")
devtools::install_github("garthtarr/mplot")
library(mplot)
```

## Usage

A reference manual is available at [garthtarr.github.io/mplot](https://garthtarr.github.io/mplot/)

## Citation

If you use this package to inform your model selection choices, please use the following citation:

- Tarr G, Müller S and Welsh AH (2018). "mplot: An R Package for Graphical Model Stability and Variable Selection Procedures." _Journal of Statistical Software_, **83**(9), pp. 1–28. doi: 10.18637/jss.v083.i09.

From R you can use:

```s
citation("mplot")
toBibtex(citation("mplot"))
```

