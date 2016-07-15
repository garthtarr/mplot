# mplot: graphical model stability and variable selection procedures

[![Travis-CI Build Status](https://travis-ci.org/garthtarr/mplot.svg?branch=master)](https://travis-ci.org/garthtarr/mplot) [![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/mplot)](http://cran.r-project.org/package=mplot) [![](http://cranlogs.r-pkg.org/badges/mplot)](http://cran.rstudio.com/web/packages/mplot/index.html) [![DL_Total](http://cranlogs.r-pkg.org/badges/grand-total/mplot?color=blue)](http://cran.rstudio.com/web/packages/mplot/index.html)

The `mplot` package provides a collection of functions designed for exploratory model selection.

We implement model stability and variable importance plots ([Mueller and Welsh (2010)](http://doi.org/10.1111/j.1751-5823.2010.00108.x); [Murray, Heritier and Mueller (2013)](http://doi.org/10.1002/sim.5855)) as well as the adaptive fence ([Jiang et al. (2008)](http://doi.org/10.1214/07-AOS517); [Jiang et al. (2009)](http://doi.org/10.1016/j.spl.2008.10.014)) for linear and generalised linear models.   We address many practical implementation issues with sensible defaults and interactive graphics to highlight model selection stability.  The speed of implementation comes from the leaps package and multicore support for bootstrapping.

The `mplot` currently only supports linear and generalised linear models, however work is progressing to incorporate survival models and mixed models.

You can see an example of the output [here](http://garthtarr.com/apps/mplot/).

## Installation

Check that you're running the most recent versions of your currently installed R packages:

```s
update.packages()
```

### Stable release on CRAN

The mplot package is now on CRAN:

```s
install.packages("mplot")
require("mplot")
```

### Development version on Github

You can use the `devtools` package to install the development version of `mplot` from [Github](https://github.com/garthtarr/mplot):

```s
# install.packages("devtools")
devtools::install_github("garthtarr/mplot")
require(mplot)
```

## Usage

```s
require(mplot)
?af
?vis
?mplot
```

## Citation

If you use this package to inform your model selection choices, please use the following citation:

* Tarr G, Mueller S and Welsh A (2015). “mplot: An R package for graphical model
stability and variable selection.” arXiv:1509.07583 [stat.ME], < URL:
http://arxiv.org/abs/1509.07583 >.

From R you can use:

```s
citation("mplot")
toBibtex(citation("mplot"))
```

## Uninstall

After you've used the development version you may like to remove it to avoid any potential conflicts in the future:

```s
detach("package:mplot", unload=TRUE)
remove.packages("mplot")
```

If you do this, then the next time you want to use it, you'll need to install the package again.
