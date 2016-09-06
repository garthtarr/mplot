---
title: Birth weight example
tags:
  - example
  - linear models
keywords: "example, AF, VIP, MSP, linear model"
last_updated: "August 31, 2016"
summary: "Birth weight example."
series: "Examples"
weight: 10
sidebar: mplot_sidebar
permalink: mplot_birthweight.html
folder: mplot
---


The `birthwt` dataset from the **MASS** package has data on 189 births at the Baystate Medical Centre, Springfield, Massachusetts during 1986 ([Venables and Ripley (2002)](http://www.springer.com/us/book/9780387954578 "Venables WN, Ripley BD (2002). Modern Applied Statistics with S. Fourth edition. Springer-Verlag, New York.")).  The main variable of  interest is low birth weight, a binary response variable `low` ([Hosmer D, Lemeshow (1989)](http://www.wiley.com/WileyCDA/WileyTitle/productCd-0470582472.html "Hosmer D, Lemeshow S (1989). Applied Logistic Regression. John Wiley & Sons, New York.")).  We have taken the same approach to modelling the full model as in \citet[pp.\ 194--197]{Venables:2002}, where `ptl` is reduced to a binary indicator of past history and `ftv` is reduced to a factor with three levels.

```r
require(MASS)
bwt <- with(birthwt, {
  race <- factor(race, labels = c("white", "black", "other"))
  ptd <- factor(ptl > 0)
  ftv <- factor(ftv)
  levels(ftv)[-(1:2)] <- "2+"
  data.frame(low = factor(low), age, lwt, race, smoke = (smoke > 0), ptd, ht = (ht > 0), ui = (ui > 0), ftv)
})
options(contrasts = c("contr.treatment", "contr.poly"))
bw.glm <- glm(low ~ ., family = binomial, data = bwt)
round(summary(bw.glm)$coef, 2)
```

```
            Estimate Std. Error z value Pr(>|z|)
(Intercept)     0.82       1.24    0.66     0.51
age            -0.04       0.04   -0.96     0.34
lwt            -0.02       0.01   -2.21     0.03
raceblack       1.19       0.54    2.22     0.03
raceother       0.74       0.46    1.60     0.11
smokeTRUE       0.76       0.43    1.78     0.08
ptdTRUE         1.34       0.48    2.80     0.01
htTRUE          1.91       0.72    2.65     0.01
uiTRUE          0.68       0.46    1.46     0.14
ftv1           -0.44       0.48   -0.91     0.36
ftv2+           0.18       0.46    0.39     0.69
```

\begin{figure}
\centering
\includegraphics[width=0.9\textwidth]{figs/bwvis.png}
\includegraphics[width=0.9\textwidth]{figs/bwboot.png}
\includegraphics[width=0.9\textwidth]{figs/bwaf.png}
\caption{Birth weight example.}
\label{fig:birthwt}
\end{figure}

The `vis` and `af` objects are generated using the fitted full model object as an argument to the `vis()` and `af()` functions.  The results are shown in Figure \ref{fig:birthwt}, where screenshots have been taken of the interactive plots because they display the larger set of variables more clearly than the static plot methods.

```r
af.bw = af(bw.glm, B = 150, c.max = 20, n.c = 40)
vis.bw = vis(bw.glm, B = 150)
plot(vis.bw, which = "vip")
plot(vis.bw, which = "boot", highlight = "htTRUE")
plot(af.bw)
print(vis.bw, min.prob = 0.15)
```

```
                  name prob logLikelihood
                 low~1 1.00       -117.34
           low~ptdTRUE 0.53       -110.95
       low~age+ptdTRUE 0.15       -108.65
low~lwt+ptdTRUE+htTRUE 0.16       -105.06
...
```

In this example, it is far less clear which is the best model, or if indeed a ``best model'' exists.  All the curves in the variable inclusion plot lie above the redundant variable curve, with `ftv2+` the least important variable.  It is possible to infer an ordering of variable importance from the variable inclusion plots, but there is no clear cutoff as to which variables should be included and which should be excluded.  This is also clear in the model stability plots, where apart from the bivariate regression with `ptd`, there are no obviously dominant models. 

In the adaptive fence plot, the only model more complex than a single covariate regression model that shows up with some regularity is the model with `lwt`, `ptd` and `ht`, though at such low levels, it is just barely a region of stability.  This model also stands out slightly in the model stability plot, where it is selected in 16% of bootstrap resamples and has a slightly lower description loss than other models of the same dimension.  It is worth recalling that the bootstrap resamples generated for the adaptive fence are separate from those generated for the model stability plots.  Indeed the adaptive fence procedure relies on a parametric bootstrap, whereas the model stability plots rely on an exponential weighted bootstrap.  Thus, to find some agreement between these methods is reassuring.

Stepwise approaches using AIC or BIC yield conflicting models, depending on whether the search starts with the full model or the null model.  As expected, the BIC stepwise approach returns smaller models than AIC, selecting the single covariate logistic regression, `low ~ ptd`, in the forward direction and the larger model, `low ~ lwt + ptd + ht` when stepping backwards from the full model.  Forward selection from the null model with the AIC yielded `low ~ ptd + age + ht + lwt + ui` whereas backward selection the slightly larger model, `low ~ lwt + race + smoke + ptd + ht + ui`.   Some of these models appear as features in the model stability plots.  Most notably the dominant single covariate logistic regression and the model with `lwt`, `ptd` and `ht` identified as a possible region of stability in the adaptive fence plot.  The larger models identified by the AIC are reflective of the variable importance plot in that they show there may still be important information contained in a number of other variables not identified by the BIC approach.

[Calcagno and de Mazancourt (2010)](http://www.jstatsoft.org/v34/i12 "Calcagno V, de Mazancourt C (2010). “glmulti: An R Package for Easy Automated Model Selection with (Generalized) Linear Models.” Journal of Statistical Software, 34(12), 1–29.") also consider this data set, but they allow for the possibility of interaction terms.  Using their approach, they identify ``two'' best models

```
low ~ smoke + ptd + ht + ui + ftv + age + lwt + ui:smoke + ftv:age
low ~ smoke + ptd + ht + ui + ftv + age + lwt + ui:smoke + ui:ht + ftv:age
```

As a general rule, we would warn against the `.*.` approach, where all possible interaction terms are considered, as it does not consider whether or not the interaction terms actually make practical sense.  [Calcagno and de Mazancourt (2010)](http://www.jstatsoft.org/v34/i12 "Calcagno V, de Mazancourt C (2010). “glmulti: An R Package for Easy Automated Model Selection with (Generalized) Linear Models.” Journal of Statistical Software, 34(12), 1–29.")  conclude that "Having two best models and not one is an extreme case where taking model selection uncertainty into account rather than looking for a single best model is certainly recommended!"  The issue here is that the software did not highlight that these models are  identical as the `ui:ht` interaction variable is simply a vector of ones, and as such, is ignored by the GLM fitting routine.

As computation time can be an issue for GLMs, it is useful to approximate the results using weighted least squares ([Hosmer, Jovanovic and Lemeshow (1989)](http://doi.org/10.2307/2531779 "Hosmer DW, Jovanovic B, Lemeshow S (1989). “Best Subsets Logistic Regression.” Biometrics, 45(4), 1265–1270.")).  In practice this can be done by fitting the logistic regression and extracting the estimated logistic probabilities, $\hat{\pi}_{i}$.  A new dependent variable is then constructed,
$$z_{i} = \log\left(\frac{\hat{\pi}_{i}}{1-\hat{\pi}_{i}}\right)  + \frac{y_{i}-\hat{\pi}_{i}}{\hat{\pi}_{i}(1-\hat{\pi}_{i})},$$ 
along with observation weights $v_{i}=\hat{\pi}_{i}(1-\hat{\pi}_{i})$. For any submodel $\alpha$ this approach produces the approximate coefficient estimates of [Lawless and Singhal (1978)](http://doi.org/10.2307/2530022 "Lawless JF, Singhal K (1978). “Efficient Screening of Nonnormal Regression Models.” Biometrics, 34(2), 318–327.") and enables us to use the **leaps** package to perform the computations for best subsets logistic regression as follows.

```r
pihat = bw.glm$fitted.values
r = bw.glm$residuals 
z = log(pihat/(1 - pihat)) + r
v = pihat*(1 - pihat)
nbwt = bwt
nbwt$z = z
nbwt$low = NULL
bw.lm = lm(z ~ ., data = nbwt, weights = v)
bw.lm.vis = vis(bw.lm, B = 150)
bw.lm.af = af(bw.lm, B = 150 c.max = 20, n.c = 40)
plot(bw.lm.vis, which = "vip")
plot(bw.lm.vis, which = "boot", highlight = "htTRUE")
plot(bw.lm.af)
```

The coefficients from `bw.lm`  are identical to `bw.glm`.  This approximation provides similar results, shown in Figure \ref{fig:bwtapprox}, in a fraction of the time.

\begin{figure}
\centering
\includegraphics[width=0.9\textwidth]{figs/approxvip.png}
\includegraphics[width=0.9\textwidth]{figs/approxboot.png}
\includegraphics[width=0.9\textwidth]{figs/approxaf.png}
\caption{Birth weight example with linear model approximation.}
\label{fig:bwtapprox}
\end{figure}


