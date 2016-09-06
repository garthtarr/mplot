---
title: Model selection background
keywords: mplot, getting_started
last_updated: August 31, 2016
tags: [getting_started]
summary: "Background on model selection."
sidebar: mplot_sidebar
permalink: mplot_install.html
folder: mplot
---


The methods provided by the **mplot** package rely heavily on various bootstrap techniques to give an indication of the stability of selecting a given model or variable and even though not done here, could be implemented with resampling methods other than the bootstrap, for example cross validation. The **m** in **mplot** stands for model selection/building and we anticipate that in  future more graphs and methods will be added to the package to further aid better and more stable building of regression models.  The intention is to encourage researchers to engage more closely with the model selection process, allowing them to pair their experience and domain specific knowledge with comprehensive summaries of the relative importance of various statistical models.

Two major challenges in model building are the vast number of models to choose from and the myriad of ways to do so.  Standard approaches include stepwise variable selection techniques and more recently the lasso. A common issue with these and other methods is their instability, that is, the tendency for small changes in the data to lead to the selection of different models.

An early and significant contribution to the use of bootstrap model selection is [Shao (2013)](http://doi.org/10.2307/2291661) who showed that carefully selecting \\(m\\) in an \\(m\\)-out-of-\\(n\\) bootstrap drives the theoretical properties of the model selector. [Mueller and Welsh (2005)](http://doi.org/10.1198/ 016214505000000529) and [Mueller and Welsh (2009)](http://www3.stat.sinica.edu.tw/sstest/oldpdf/A19n315.pdf) modified and generalised Shao's \\(m\\)-out-of-\\(n\\) bootstrap model selection method to robust settings, first in linear regression and then in generalised linear models. The bootstrap is also used in regression models that are not yet covered by the **mplot** package, such as mixed models (e.g. [Shang and Cavanaugh (2008)](http://doi.org/10.1016/j.csda.2007.06.019)) or partially linear models (e.g. [Mueller and Vial (2008)](http://doi.org/10.1111/j.1467-842X.2009.00540.x)) as well as for the selection of tuning parameters in regularisation methods (e.g. [Park, Sakaori and Konishi (2008)](http://doi.org/10.1080/00949655.2012.755532)).

Assume that we have \\(n\\) independent observations \\(\mathbf{y} = (y_{1},\ldots,y_{n})^{\top}\\) and an \\(n\times p\\) full rank design matrix \\(\mathbf{X}\\) whose columns are indexed by \\(1,\ldots,p\\). Let \\(\alpha\\) denote any subset of \\(p_{\alpha}\\) distinct elements from \\(\{1,\ldots,p\}\\). Let \\(\mathbf{X}_{\alpha}\\) be the corresponding \\(n\times p_{\alpha}\\) design matrix and \\(\mathbf{x}_{\alpha i}^{\top}\\) denote the \\(i\\)th row of \\(\mathbf{X}_{\alpha}\\).

The **mplot** package focuses specifically on linear and generalised linear models (GLM). In the context of GLMs, a model \\(\alpha\\) for the relationship between the response \\(\mathbf{y}\\) and the design matrix \\(\mathbf{X}_{\alpha}\\) is specified by
\begin{align}
\mathbb{E}(\mathbf{y}) = h(\mathbf{X}_{\alpha}^{\top}\bm{\beta}_{\alpha}), \text{ and }\operatorname{var}(\mathbf{y}) = \sigma^{2}v(h(\mathbf{X}_{\alpha}^{\top}\bm{\beta}_{\alpha})),
\end{align}
where \\(\bm{\beta}_{\alpha}\\) is an unknown \\(p_{\alpha}\\)-vector of regression parameters and \\(\sigma\\) is an unknown scale parameter. Here \\(\mathbb{E}(\cdot)\\) and \\(\operatorname{var}(\cdot)\\)  denote the expected value and variance of a random variable, \\(h\\) is the inverse of the usual link function and both \\(h\\) and \\(v\\) are assumed known. When \\(h\\) is the identity and \\(v(\cdot)=1\\), we recover the standard linear model.  

The purpose of model selection is to choose one or more models \\(\alpha\\) from a set of candidate models, which may be the set of all models \\(\mathcal{A}\\) or a reduced model set (obtained, for example, using any initial screening method). Many model selection procedures assess model fit using the generalised information criterion,
\begin{equation}
\textrm{GIC}(\alpha,\lambda) = \hat{Q}(\alpha) + \lambda p_{\alpha}. \label{GIC}
\end{equation}
The \\(\hat{Q}(\alpha)\\) component is a measure of _description loss_ or _lack of fit_, a function that describes how well a model fits the data, for example, the residual sum of squares or \\(-2~\times~\text{log-likelihood}\\). The number of independent regression model parameters, \\(p_{\alpha}\\), is a measure of _model complexity_. The penalty multiplier, \\(\lambda\\), determines the properties of the model selection criterion \citep{Mueller:2013,Mueller:2010}. Special cases, when \\(\hat{Q}(\alpha)=-2\times\text{log-likelihood}(\alpha)\\), include the AIC with \\(\lambda=2\\), BIC with \\(\lambda=\log(n)\\) and more generally the generalised information criterion (GIC) with \\(\lambda\in\mathbb{R}\\) [Konishi and Kitagawa (2008)](http://doi.org/10.1093/biomet/83.4.875 "Konishi S, Kitagawa G (1996). “Generalised Information Criteria in Model Selection.” Biometrika, 83(4), 875–890.").

The **mplot** package currently implements _variable inclusion plots_, _model stability plots_ and a model selection procedure inspired by the adaptive fence of [Jiang et al. (2008)](http://doi.org/10.1214/07-AOS517).  Variable inclusion plots were introduced independently by [Mueller and Welsh (2010)](http://doi.org/10.1111/j.1751-5823.2010.00108.x) and [Meinshausen and Bühlmann (2010)](http://doi.org/10.1111/j.1467-9868.2010.00740.x). The idea is that the best model is selected over a range of values of the penalty multiplier \\(\lambda\\) and the results are visualised on a plot which shows how often each variable is included in the best model.  These types of plots have previously been referred to as stability paths, model selection curves and most recently variable inclusion plots (VIPs) in [Murray, Heritier and Mueller (2013)](http://doi.org/10.1002/sim.5855).  An alternative to penalising for the number of variables in a model is to assess the fit of models within each model size. This is the approach taken in our model stability plots where searches are performed over a number of bootstrap replications and the best models for each size are tallied. The rationale is that if there exists a _correct_ model of a particular model size it will be selected overwhelmingly more often than other models of the same size.  Finally, the adaptive fence was introduced by [Jiang et al. (2008)](http://doi.org/10.1214/07-AOS517) to select mixed models.  This is the first time code has been made available to implement the adaptive fence and the first time the adaptive fence has been applied to linear and generalised linear models.

This article introduces three data examples that each highlight different aspects of the graphical methods made available by **mplot**. Sections \ref{sec:ie}-\ref{sec:ig} are based on a motivating example where the true data generating model is known. We use this example to highlight one of the classical failings of stepwise procedures before introducing variable inclusion plots and model stability plots through the `vis()` function in Section \ref{sec:vis}. Our implementation of the adaptive fence with the `af()` function is presented in Section \ref{sec:af}. 

For all methods, we provide publication quality classical plot methods using base **R** graphics as well as interactive plots using the **googleVis** package ([Gesmann and de Castillo (2011)](https://journal.r-project.org/archive/2011-2/RJournal_2011-2_Gesmann+de~Castillo.pdf)). In Section \ref{sec:ig}, we show how to add further utility to these plot methods by packaging the results in a **shiny** web interface which facilitates a high degree of interactivity ([Chang et al. (2015)](http://shiny.rstudio.com)).

In Section \ref{sec:timing} we show computing times in a simulation study, varying the number of variables from 5 to 50; we further illustrate the advantage of using multiple core technology.  We then show with two applied examples the practical merit of our graphical tools in Section \ref{sec:examples}.

To conclude, we highlight in Section \ref{sec:conclusion} the key contributions of the three data examples and make some final brief remarks.


{% include links.html %}
