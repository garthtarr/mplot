# mplot

The mplot package provides a collection of functions designed for exploratory model building.

Model stability curves are implemented.

The fence implements the adaptive fence routine with sensible defaults, new graphics to highlight model selection stability and experimental automated "best" model selection.  It currently only supports linear models, however work is progressing to incorporate mixed models.

The speed of implementation comes from the leaps package and multicore support for bootstrapping.

## Installation

You can install the development version from [Github](https://github.com/garthtarr/mplot):

```s
require(devtools)
install_github("mplot",username="garthtarr",auth_token="b2b362412956f9df042eb4f704d06c60f11735c3",quick=TRUE)
```

To get the most out of the mplot package, you will need a few extra packages too:

```s
install.packages(c("leaps","googleVis","foreach","doMC"))
```

## Usage

```s
require(mplot)
?mplot
# demo(mplot) not yet implemented
```

## Uninstall

After you've used the development version you may like to remove it to avoid any potential conflicts in the future with:

```s
detach("package:mplot", unload=TRUE)
remove.packages("mplot")
```

Then the next time you want to use it, you'll need to install the package again from github.

## Known issues

### Interpreting factor variables

If you have something like
```s
n = 100
set.seed(11)
e = rnorm(n)
x0 = 1
x1 = rnorm(n)
x2 = rnorm(n)
x6 = rep(c("A","B","C","D"),n/4)
x3 = x1^2
x4 = x2^2
x5 = x1*x2
gp = as.numeric(factor(x6)) 
yI = x0 + x1 + x2 + gp + e
datI = data.frame(yI,x1,x2,x3,x4,x5,x6)
af1 = af(yI~., data=datI, n.cores=2)
summary(af1)
```

The backwards stepwise procedures come from the full model where 
the factors are broken down by levels (because they start with
a model where the factors are broken down by levels).  The forwards
procedures use `s upper=as.formula(mf$model)` as their full model
however this calls the factor variable simply `s x6`.  It's not a 
huge issue in the sense that the number of parameters takes into 
account the fact that `s x6` is a factor variable, but it makes the
output look inconsistent.

What happens if one (or more) of the factor levels is not significant?
```s
gp2 = gp
gp2[gp2<=2] = 0
yI = x0 + x1 + x2 + gp2 + e
datI = data.frame(yI,x1,x2,x3,x4,x5,x6)
af1 = af(yI~., data=datI, n.cores=2)
summary(af1)
```
So this is more of an issue when not all of the dummy variables
are significant.  Not a huge issue necessarily, just makes the upper
bound on the number of parameters a bit bigger than perhaps it should
be, but really it's the lower bound that is more important, because 
that helps the fence procedure avoid picking too small models that
contain a small number of super dominant variables.
