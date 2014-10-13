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
lm1 = lm(y~., data=dat)
af1 = af(lm1, n.cores=3)
```

```
## Loading required package: doMC
## Loading required package: foreach
## foreach: simple, scalable parallel programming from Revolution Analytics
## Use Revolution R for scalability, fault tolerance and more.
## http://www.revolutionanalytics.com
## Loading required package: iterators
```


```r
summary(af1)
```

```
## 
## Call:
## af(mf = lm1, n.cores = 3)
## 
## Adaptive fence model (c*=18.8):
## y ~ x1 + x2
## 
## Model sizes considered: 1 to 6 (including intercept).
```


```r
plot(af1)
```

```
## 
## Welcome to googleVis version 0.5.5
## 
## Please read the Google API Terms of Use
## before you start using the package:
## https://developers.google.com/terms/
## 
## Note, the plot method of googleVis will by default use
## the standard browser to display its output.
## 
## See the googleVis package vignettes for more details,
## or visit http://github.com/mages/googleVis.
## 
## To suppress this message use:
## suppressPackageStartupMessages(library(googleVis))
```


















