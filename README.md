# mplot

The mplot package provides a collection of functions designed for exploratory model selection.

We implement the adaptive fence routine with sensible defaults, new graphics to highlight model selection stability.  It currently only supports linear and generalised linear models, however work is progressing to incorporate survival models and mixed models.

The speed of implementation comes from the leaps package and multicore support for bootstrapping.

You can see an example of the output [here](http://128.199.224.73/mplot-diabetes/).

## Installation

Check that you're running the most recent versions of your currently installed R packages:

```s
update.packages()
```

The `mplot()` function requires the development version of `shinydashboard` (v0.4.0.9000 or greater).  This is expected to be on CRAN in a couple of weeks time, but for now you can install it using:

```s
install.packages("devtools")
devtools::install_github("rstudio/shinydashboard")
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


## Uninstall

After you've used the development version you may like to remove it to avoid any potential conflicts in the future:

```s
detach("package:mplot", unload=TRUE)
remove.packages("mplot")
```

If you do this, then the next time you want to use it, you'll need to install the package again.
