# mplot

The mplot package provides a collection of functions designed for exploratory model sleection.

We implement the adaptive fence routine with sensible defaults, new graphics to highlight model selection stability and experimental automated "best" model selection.  It currently only supports linear and generalised linear models, however work is progressing to incorporate mixed models.

The speed of implementation comes from the leaps package and multicore support for bootstrapping.

## Installation

You can install the development version from [Github](https://github.com/garthtarr/mplot):

```s
require(devtools)
install_github("garthtarr/mplot",quick=TRUE)
```

To get the most out of the mplot package, you will need a few extra packages too:

```s
install.packages(c("leaps","googleVis","foreach","doMC"))
```

## Usage

```s
require(mplot)
?mplot
vignette("mplot-guide",package="mplot")
vignette("mplot-stepwise",package="mplot")
```

## Uninstall

After you've used the development version you may like to remove it to avoid any potential conflicts in the future with:

```s
detach("package:mplot", unload=TRUE)
remove.packages("mplot")
```

Then the next time you want to use it, you'll need to install the package again from github.

## Known issues

The `vis` function is not yet optimised to take advantage of leaps
technology and so it can take a while to run on moderately large data sets.
