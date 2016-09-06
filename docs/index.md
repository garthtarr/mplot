---
title: Getting started with mplot
keywords: sample homepage
tags: [getting_started]
sidebar: mydoc_sidebar
permalink: index.html
summary: This website will help you get started with mplot. The topics covered in this documentation provide information  about working with many aspects of the mplot package.
---

The **mplot** package provides an easy to use implementation of model stability and variable inclusion plots ([Mueller and Welsh (2010)](http://doi.org/10.1111/j.1751-5823.2010.00108.x); [Murray, Heritier and Mueller (2013)](http://doi.org/10.1002/sim.5855)) as well as the adaptive fence ([Jiang et al. (2008)](http://doi.org/10.1214/07-AOS517); [Jiang et al. (2009)](http://doi.org/10.1016/j.spl.2008.10.014)) for linear and generalised linear models. We provide a number of innovations on the standard procedures and address many practical implementation issues including the addition of redundant variables, interactive visualisations and approximating logistic models with linear models. An option is provided that combines our bootstrap approach with **glmnet** for higher dimensional models.  The plots and graphical user interface leverage state of the art web technologies to facilitate interaction with the results. The speed of implementation comes from the **leaps** package and cross-platform multicore support.

The main contributions of the **mplot** package are model stability plots and variable inclusion plots, implemented through the `vis()` function, and the simplified adaptive fence for linear and generalised linear models via the `af()`.

Our methods generate large amounts of raw data about the fitted models.  While the print and summary output from both functions provide suggestions as to which models appear to be performing best, it is not our intention to have researchers simply read off the _best_ model from the output.  The primary purpose of these techniques is to help inform a researcher's model selection choice. As such, the real value in using these functions is in the extensive plot methods provided that help visualise the results and get new insights.  This is reflected in the choice of name `vis`, short for visualise, as this is the ultimate goal -- to visualise the stability of the model selection process.  

The **mplot** package currently only supports linear and generalised linear models, however work is progressing to incorporate survival models and mixed models.

You can see an example of the output [here](http://garthtarr.com/apps/mplot/).







{% include links.html %}
