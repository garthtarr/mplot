---
title: "A brief introduction to mplot"
author: "Garth Tarr"
date: "`s Sys.Date()`"
output: html_document
---

<!--
%\VignetteEngine{knitr::knitr}
%\VignetteIndexEntry{A brief introduction to mplot}
-->

# A brief introduction to mplot

The **mplot** package implements a range of model selection procedures and model stability plots designed to provide users with the information they need to make informed decisions about model selection issues in linear and mixed models.

There are two main componenets: model selection via the adaptive fence procedure (Jiang et. al., 2009) and model stability curves as described in Müller and Welsh (2010).

## Adaptive fence

The adaptive fence procedure is ... super brief recap of theory here.

### Artificial example

The adaptive fence procedure is (currently; in future it will be wrapped into the higher level `fencer` function) implemented through the function `af` as follows:


```r
require(mplot,quietly=TRUE)
op = options(gvis.plot.tag = "chart")
n = 100
set.seed(11)
e = rnorm(n)
x0 = 1
x1 = rnorm(n)
x2 = rnorm(n)
x3 = x1^2
x4 = x2^2
x5 = x1*x2
y = x0 + x1 + x2 + e
dat = data.frame(y,x1,x2,x3,x4,x5)
af1 = af(y~., data=dat, n.cores=3)
```

```
## Loading required package: doMC
## Loading required package: foreach
## Loading required package: iterators
## Loading required package: parallel
```


```r
summary(af1)
```

```
## 
## Call:
## af(fixed = y ~ ., data = dat, n.cores = 3)
## 
## Adaptive fence model (c*=18.3):
## y ~ x1 + x2
## 
## Model sizes considered: 2 to 5 (including intercept).
## 
## Stepwise procedures:
## Forwards AIC: y ~ x1 + x2 + x3
## Backwards AIC: y ~ x1 + x2 + x3
## Forwards BIC: y ~ x1 + x2
## Backwards BIC: y ~ x1 + x2
```



```r
plot(af1)
```

```
## Loading required package: googleVis
## 
## Welcome to googleVis version 0.5.2
## 
## Please read the Google API Terms of Use
## before you use the package:
## https://developers.google.com/terms/
## 
## Note, the plot method of googleVis will by default use
## the standard browser to display its output.
## 
## See the googleVis package vignettes for more details.
## 
## To suppress the this message use:
## suppressPackageStartupMessages(library(googleVis))
## 
## starting httpd help server ... done
```


In this example, we can see that the first (and only) peak in the plot of $p^*$ against $c$ occurs for the model of $y$ against $x_1$ and $x_2$.  The $c^*$ value is 18.3, and the model selected as a result of using that $c^*$ value is given in the summary output.  Note that the range of $c$ was predetermined by considering models selected using an initial application of forward and backward searches.

### Real world example

If there exists a *true* model, the adaptive fence tends to find it quite easily.  In practice, there is rarely such a well defined and readily identifiable data generating process and it is the role of the investigator to make a judgement call as to which model *best* describes the underlying process.  A nice feature of the adaptive fence procedure is its ability to guide the investigator and highlight plausible candidate models.

Consider the body fat example of CITATION HERE.  


```r
bfat.full = af(Bodyfat~.,data=bodyfat,n.cores=3, n.c=100,initial.stepwise=FALSE)
plot(bfat.full)
```


Note that if we consider the full range of possible values for the parameter $c$, by setting `initial.stepwise=FALSE`, it appears that there is one dominant variable, `abdo`, and hence naiively, one might chose a simple linear regression of `Bodyfat` on `abdo`.

Instead, one might like to incorporate some additional information to better inform our choice.  In particular, let us consider the number of variables that AIC and BIC forward and backward stepwise procedures and investigate only models of similar size.


```r
bfat.stepwise = af(Bodyfat~.,data=bodyfat,n.cores=3, n.c=50,initial.stepwise=TRUE)
plot(bfat.stepwise)
```


Doing this, we focus attention on smaller $c$ values, i.e. larger model sizes.  We can see that the regression of `Bodyfat` on `weight` and `abdo` is chosen over a large range of c values.  However, it is also important to note that in this range, the pstar values are all quite low, suggesting that there is no one dominant model that *best* describes the data generating process.


```r
summary(bfat.stepwise)
```

```
## 
## Call:
## af(fixed = Bodyfat ~ ., data = bodyfat, n.cores = 3, n.c = 50, 
##     initial.stepwise = TRUE)
## 
## Adaptive fence model (c*=18.3):
## Bodyfat ~ Abdo
## 
## Model sizes considered: 2 to 6 (including intercept).
## 
## Stepwise procedures:
## Forwards AIC: Bodyfat ~ Abdo + Weight + Neck + Fore
## Backwards AIC: Bodyfat ~ Weight + Neck + Abdo + Fore
## Forwards BIC: Bodyfat ~ Abdo + Weight
## Backwards BIC: Bodyfat ~ Weight + Abdo
```


### Mixed model example

Yet to be implemented.

## Model selection curves

The standard plots are the model stability and variable inclusion plots.  Both of these are performed using the (poorly named) `lvp()` function.


```r
require(mvtnorm)
```

```
## Loading required package: mvtnorm
```

```r
n = 250
set.seed(11)
p = 7
cor.mat = diag(p)
cor.mat[1, 2] = cor.mat[2, 1] = 0.7
cor.mat[5, 6] = cor.mat[6, 5] = 0.7
X = rmvnorm(n, sigma = cor.mat)
beta = c(1, 2.1, 0, 1.5, 0, 0, 0.9, 1.1)
z = cbind(1, X) %*% beta
pr = 1/(1 + exp(-z))
y = rbinom(n, 1, pr)
df = data.frame(y, X)
lvp1 = lvp(fixed = y ~ ., data = df, family = "binomial", B = 50)
```

```
## Error: could not find function "lvp"
```


### Model stability plots

There are two variants here.  The first simply iterates over all possible models at each model size and returns a scatter plot showing a goodness of fit measure (-2*loglik) against model size.


```r
plot(lvp1, which = "msc")
```

```
## Error: object 'lvp1' not found
```


The second, performs a simple weighted bootstrap to see which models perform *best* at conditional on the model size.


```r
plot(lvp1, which = "msc")
```

```
## Error: object 'lvp1' not found
```


### Variable inclusion plots

The variable inclusion plots use that same bootstrap simulations as the second model selection curve plot and identifies how often each variable is selected as being in the *best* model for a given penalty.


```r
plot(lvp1, which = "vip")
```

```
## Error: object 'lvp1' not found
```

