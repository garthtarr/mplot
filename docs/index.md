---
title: Getting started with mplot
keywords: sample homepage
tags: [getting_started]
sidebar: mydoc_sidebar
permalink: index.html
summary: These brief instructions will help you get started quickly with mplot. The other topics in this help provide additional information and detail about working with other aspects of the mplot package.
---

The `mplot` package provides a collection of functions designed for exploratory model selection.

We implement model stability and variable importance plots ([Mueller and Welsh (2010)](http://doi.org/10.1111/j.1751-5823.2010.00108.x); [Murray, Heritier and Mueller (2013)](http://doi.org/10.1002/sim.5855)) as well as the adaptive fence ([Jiang et al. (2008)](http://doi.org/10.1214/07-AOS517); [Jiang et al. (2009)](http://doi.org/10.1016/j.spl.2008.10.014)) for linear and generalised linear models.   We address many practical implementation issues with sensible defaults and interactive graphics to highlight model selection stability.  The speed of implementation comes from the leaps package and multicore support for bootstrapping.

The `mplot` currently only supports linear and generalised linear models, however work is progressing to incorporate survival models and mixed models.

You can see an example of the output [here](http://garthtarr.com/apps/mplot/).



## Usage

```s
require(mplot)
?af
?vis
?mplot
```




{% include links.html %}
