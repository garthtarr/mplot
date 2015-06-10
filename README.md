# mplot

The mplot package provides a collection of functions designed for exploratory model selection.

We implement the adaptive fence routine with sensible defaults, new graphics to highlight model selection stability and experimental automated "best" model selection.  It currently only supports linear and generalised linear models, however work is progressing to incorporate mixed models.

The speed of implementation comes from the leaps package and multicore support for bootstrapping.

## Installation

Step 1: check that you're running the most recent versions of your currently installed R packages:

```s
update.packages()
```

Step 2: use the `devtools` package to install the development version of `mplot` from [Github](https://github.com/garthtarr/mplot):

```s
# install.packages("devtools")
require(devtools)
install_github("garthtarr/mplot",quick=TRUE)
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

After you've used the development version you may like to remove it to avoid any potential conflicts in the future with:

```s
detach("package:mplot", unload=TRUE)
remove.packages("mplot")
```

If you do this, then the next time you want to use it, you'll need to install the package again from github.
