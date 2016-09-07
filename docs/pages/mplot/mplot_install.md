---
title: Installation
keywords: install, mplot, load, getting_started
last_updated: August 31, 2016
tags: [getting_started]
summary: "Installing the mplot package."
sidebar: mplot_sidebar
permalink: mplot_install.html
toc: false
folder: mplot
---


Check that you're running the most recent versions of your currently installed R packages:

```s
update.packages()
```

### Stable release on CRAN

The mplot package has been on [CRAN](https://cran.r-project.org/package=mplot) since June 2015.  You can install it from CRAN in the usual way:

```s
install.packages("mplot")
require("mplot")
```

### Development version on GitHub

You can use the **devtools** package to install the development version of **mplot** from [GitHub](https://github.com/garthtarr/mplot):

```s
# install.packages("devtools")
devtools::install_github("garthtarr/mplot")
require(mplot)
```

{% include links.html %}
