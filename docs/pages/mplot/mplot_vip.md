---
title: Variable inclusion plots
tags:
  - VIP
keywords: "VIP, variable inclusion plots"
last_updated: "August 31, 2016"
summary: "Overview of variable inclusion plots."
series: "Variable inclusion plots"
weight: 10
sidebar: mplot_sidebar
permalink: mplot_vip.html
folder: mplot
---

Rather than visualising a loss measure against model size, it can be instructive to consider which variables are present in the overall _best_ model over a set of bootstrap replications.  To facilitate comparison between models of different sizes we use the generalised information criterion, 

$$\textrm{GIC}(\alpha,\lambda) = \hat{Q}(\alpha) + \lambda p_{\alpha}.$$

The \\(\hat{Q}(\alpha)\\) component is a measure of _description loss_ or _lack of fit_, a function that describes how well a model fits the data, for example, the residual sum of squares or \\(-2~\times~\text{log-likelihood}\\). The number of independent regression model parameters, \\(p\_{\alpha}\\), is a measure of _model complexity_. The penalty multiplier, \\(\lambda\\), determines the properties of the model selection criterion \citep{Mueller:2013,Mueller:2010}. Special cases, when \\(\hat{Q}(\alpha)=-2\times\text{log-likelihood}(\alpha)\\), include the AIC with \\(\lambda=2\\), BIC with \\(\lambda=\log(n)\\) and more generally the generalised information criterion (GIC) with \\(\lambda\in\mathbb{R}\\) \citep{Konishi:1996}.

Using the same exponential weighted bootstrap replications as in the model selection plots, we have a set of \\(B\\) bootstrap replications and for each model size we know which model has the smallest description loss.   This information is used to determine which model minimises the GIC over a range of values of the penalty parameter, \\(\lambda\\), in each bootstrap sample.  For each value of \\(\lambda\\), we extract the variables present in the _best_ models over the \\(B\\) bootstrap replications and calculate the corresponding bootstrap probabilities that a given variable is present.  These calculations are visualised in a variable inclusion plot (VIP) as introduced by [Mueller and Welsh (2010)](http://dx.doi.org/10.1111/j.1751-5823.2010.00108.x) and [Murray, Heritier and Mueller (2013)](http://dx.doi.org/10.1002/sim.5855). The VIP shows empirical inclusion probabilities as a function of the penalty multiplier \\(\lambda\\). The probabilities are calculated by observing how often each variable is retained in \\(B\\) exponential weighted bootstrap replications.  Specifically, for each bootstrap sample \\(b=1,\ldots,B\\) and each penalty multiplier \\(\lambda\\), the chosen model, \\(\hat{\alpha}\_{\lambda}^{b}\in \mathcal{A}\\), is that which achieves the smallest \\(\textrm{GIC}(\alpha,\lambda;\mathbf{w}\_b) = \hat{Q}^b(\alpha)+\lambda p\_{\alpha}\\), where \\(\mathbf{w}\_b\\) is the \\(n\\)-vector of independent and identically distributed exponential weights. The inclusion probability for variable \\(x\_{j}\\) is estimated by \\(B^{-1}\sum_{i=1}^{B}\mathbb{I}\\{j\in \hat{\alpha}\_{\lambda}^{b}\\}\\), where \\(\mathbb{I}\\{j\in \hat{\alpha}\_{\lambda}^{b}\\}\\) is one if \\(x\_{j}\\) is in the final model and zero otherwise.  Following \citet{Murray:2013}, the default range of \\(\lambda\\) values is \\(\lambda\in[0,2\log(n)]\\) as this includes most standard values used for the penalty parameter.

The example shown in the bottom panel of Figure \ref{plot.vis} is obtained using the `which = "vip"` argument to the plot function.  As expected, when the penalty parameter is equal to zero, all variables are included in the model;  the full model achieves the lowest description loss, and hence minimises the GIC when there is no penalisation.  As the penalty parameter increases, the inclusion probabilities for individual variables typically decrease as more parsimonious models are preferred.  In the present example, the inclusion probabilities for the \\(x\_8\\) variable exhibit a sharp decrease at low levels of the penalty parameter, but then increase steadily as a more parsimonious model is sought.  This pattern helps to explain why stepwise model selection chose the larger model with all the variables except \\(x\_8\\) -- there exists a local minimum.  Hence, for large models the inclusion of \\(x\_8\\) adds no additional value over having all the other explanatory variables in the model.

It is often instructive to visualise how the inclusion probabilities change over the range of penalty parameters.  The ordering of the variables in the legend corresponds to their average inclusion probability over the whole range of penalty values.  We have also added an independent standard Gaussian random variable to the model matrix as a redundant variable (`RV`).  This provides a baseline to help determine which inclusion probabilities are _significant_ in the sense that they exhibit a different behaviour to the `RV` curve.  Variables with inclusion probabilities near or below the `RV` curve can be considered to have been included by chance.  

To summarise, VIPs continue the model stability theme. Rather than simply using a single penalty parameter associated with a particular information criterion, for example the AIC with \\(\lambda=2\\), our implementation of VIPs adds considerable value by allowing us to learn from a range of penalty parameters.  Furthermore, we are able to see which variables are most often included over a number of bootstrap samples. 

