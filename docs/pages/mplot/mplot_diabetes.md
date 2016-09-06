---
title: Diabetes example
tags:
  - example
  - linear models
keywords: "example, AF, VIP, MSP, linear model"
last_updated: "August 31, 2016"
summary: "Diabetes example."
series: "Examples"
weight: 10
sidebar: mplot_sidebar
permalink: mplot_diabetes.html
folder: mplot
---


Table \ref{tab:diabetes} shows a subset of the diabetes data used in \citet{Efron:2004}.  There are 10 explanatory variables, including age (\code{age}), sex (\code{sex}), body mass index (\code{bmi}) and mean arterial blood pressure (\code{map}) of 442 patients as well as six blood serum measurements (\code{tc}, \code{ldl}, \code{hdl}, \code{tch}, \code{ltg} and \code{glu}).  The response is a measure of disease progression one year after the baseline measurements. 

\begin{table}[t]
\centering
\begin{tabular*}{\textwidth}{@{\centering\extracolsep{\fill}}rcccR[.][]{3}{0}cR[.][.]{3}{1}ccccR[,][]{3}{0}}
\toprule
 &  &  &  & \multicolumn{1}{c}{} & \multicolumn{6}{c}{Serum measurements} & \multicolumn{1}{c}{Response} \\ \cmidrule(lr){6-11}
Patient &age&sex&bmi&\multicolumn{1}{c}{map}&tc&\multicolumn{1}{c}{ldl} & hdl & tch & ltg & glu & \multicolumn{1}{c}{$y$} \\ \midrule
1 		& 59 	& 2 	& 32.1 	& 101 	& 157 	& 93.2 & 38 & 4 & 4.9 & 87 & 151 \\
2 		& 48 	& 1 	& 21.6 	& 87 	& 183 	& 103.2 & 70 & 3 & 3.9 & 69 & 75 \\
3 		& 72 	& 2 	& 30.5 	& 93 	& 156 	& 93.6 & 41 & 4 & 4.7 & 85 & 141 \\
\vdots \ \ 	&\vdots &\vdots &\vdots &\multicolumn{1}{c}{\vdots} &\vdots &\multicolumn{1}{c}{\vdots} &\vdots &\vdots &\vdots &\vdots &\multicolumn{1}{c}{\vdots} \\
441 & 36 & 1 & 30.0 & 95 & 201 & 125.2 & 42 & 5 & 5.1 & 85 & 220 \\
442 & 36 & 1 & 19.6 & 71 & 250 & 133.2 & 97 & 3 & 4.6 & 92 & 57 \\ \bottomrule
\end{tabular*}
\caption{Measurements on 442 diabetes patients over 10 potential predictor variables and the response variable, a measure of disease progression after one year.}
\label{tab:diabetes}
\end{table} 

Figure \ref{fig:diabetesmain} shows the results of the main methods for the diabetes data obtained using the following code.

\begin{CodeChunk}
\begin{CodeInput}
R> lm.d = lm(y ~ ., data = diabetes)
R> vis.d = vis(lm.d, B = 200)
R> af.d = af(lm.d, B = 200, n.c = 100, c.max = 100)
R> plot(vis.d, interactive = FALSE, which = "vip")
R> plot(vis.d, interactive = FALSE, which = "boot", max.circle = 0.25,
+    highlight = "hdl")
R> plot(af.d, interactive = FALSE, best.only = TRUE, 
+    legend.position = "bottomright")
R> plot(af.d, interactive = FALSE, best.only = FALSE)
\end{CodeInput}
\end{CodeChunk}

A striking feature of the variable inclusion plot is the non-monotonic nature of the \code{hdl} line.   As the penalty value increases, and a more parsimonious model is sought, the \code{hdl} variable is selected more frequently while at the same time other variables with similar information are dropped.  Such paths occur when a group of variables contains similar information to another variable.  The \code{hdl} line is a less extreme example of what occurs with $x_8$ in the artificial example (see Figure \ref{plot.vis}).   The path for the age variable lies below the path for the redundant variable, indicating that it does not provide any useful information. The \code{bmi} and \code{ltg} paths are horizontal with a bootstrap probability of 1 for all penalty values indicating that they are very important variables, as are \code{map} and \code{sex}.  From the variable inclusion plot alone, it is not obvious whether \code{tc} or \code{hdl} is the next most important variable.  Some guidance on this issue is provided by the model stability and adaptive fence plots.

In order to determine which circles correspond to which models in the static version of the bootstrap stability plot, we need to consult the print output of the \code{vis} object.

\begin{CodeChunk}
\begin{CodeInput}
R> vis.d
\end{CodeInput}
\begin{CodeOutput}
                     name prob logLikelihood
                      y~1 1.00      -2547.17
                    y~bmi 0.73      -2454.02
                y~bmi+ltg 1.00      -2411.20
            y~bmi+map+ltg 0.69      -2402.61
         y~bmi+map+tc+ltg 0.42      -2397.48
        y~bmi+map+hdl+ltg 0.32      -2397.71
    y~sex+bmi+map+hdl+ltg 0.67      -2390.13
 y~sex+bmi+map+tc+ldl+ltg 0.47      -2387.30
\end{CodeOutput}
\end{CodeChunk}



As in the variable inclusion plots, it is clear that the two most important variables are \code{bmi} and \code{ltg}, and the third most important variable is  \code{map}.  In models of size four (including the intercept), the model with \code{bmi}, \code{ltg} and \code{map} was selected in 69\% of bootstrap resamples.  There is no clear dominant model in models of size five, with \code{tc} and \code{hdl} both competing to be included.  In models of size six, the combination of \code{sex} and \code{hdl} with the core variables \code{bmi}, \code{map} and \code{ltg}, is the most stable option; it is selected in 67\% of bootstrap resamples.  As the size of the model space in dimension six is much larger than the size of the model space for dimension four, it could be suggested that the 0.67 empirical probability for the \{\code{bmi}, \code{map}, \code{ltg}, \code{sex}, \code{hdl}\} model is a stronger result than the 0.69 result for the \{\code{bmi}, \code{ltg}, \code{map}\} model. 

The adaptive fence plots in the bottom row of Figure \ref{fig:diabetesmain} show a clear peak for the model with just \code{bmi} and \code{ltg}. There are two larger models that also occupy regions of stability, albeit with much lower peaks.  These are \{\code{bmi}, \code{map}, \code{ltg}\} and \{\code{bmi}, \code{map}, \code{ltg}, \code{sex}, \code{hdl}\} which also showed up as dominant models in the model stability plots.  Contrasting  \code{best.only = TRUE} in the lower left panel with  \code{best.only = FALSE} in the lower right panel, we can see that the peaks tend to be more clearly distinguished, though the regions of stability remain largely unchanged.

Stepwise approaches using a forward search or backward search with the AIC or BIC all yield a model with \{\code{bmi}, \code{map}, \code{ltg}, \code{sex}, \code{ldl}, \code{tc}\}.  This model was selected 47\% of the time in models of size 7.  The agreement between the stepwise methods may be comforting for the researcher, but it does not aid a discussion about what other models may be worth exploring.

An interactive version of the plots in Figure \ref{fig:diabetesmain} is available at garthtarr.com/apps/mplot.

\begin{figure}[p]
\centering
\includegraphics[width=0.45\textwidth]{figs/dbvip.pdf}\includegraphics[width=0.45\textwidth]{figs/dbboot.pdf}
\includegraphics[width=0.45\textwidth]{figs/dbaf1.pdf}\includegraphics[width=0.45\textwidth]{figs/dbaf2.pdf}
\caption{Diabetes main effects example.}
\label{fig:diabetesmain}
\end{figure}

\begin{figure}[p]
\centering
\includegraphics[width=0.45\textwidth]{figs/dbvipint.pdf}\includegraphics[width=0.45\textwidth]{figs/dbbootint.pdf}
\includegraphics[width=0.45\textwidth]{figs/dbaf1int10.pdf}\includegraphics[width=0.45\textwidth]{figs/dbaf2int10.pdf}
\caption{Diabetes interactions terms example.}
\label{fig:diabetesint}
\end{figure}


To incorporate interaction terms, we suggest selecting the main effects first, then regressing the relevant interaction terms on the residuals from the main effects model.  This approach ensures that the main effects are always taken into account. In this example, we estimate the dominant model of dimension six and obtain the fitted residuals.  The interaction terms are then regressed on the fitted residuals. 

\begin{CodeChunk}
\begin{CodeInput}
R> lm.d.main = lm(y ~ sex + bmi + map + hdl + ltg, data = diabetes)
R> summary(lm.d.main)
R> db.main = diabetes[, c("sex", "bmi", "map", "hdl", "ltg")]
R> db.main$y = lm.d.main$residuals
R> lm.d.int = lm(y ~ .*. - sex - bmi - map - hdl - ltg, data = db.main)
R> vis.d.int = vis(lm.d.int, B = 200)
R> af.d.int = af(lm.d.int, B = 200, n.c = 100)
R> vis.d.int
\end{CodeInput}
\begin{CodeOutput}
                              name prob logLikelihood
                               y~1 1.00      -2390.13
 y~bmi.map+bmi.hdl+map.ltg+hdl.ltg 0.56      -2385.89
\end{CodeOutput}
\end{CodeChunk}

The result can be found in Figure \ref{fig:diabetesint}. The variable inclusion plots suggest that the most important interaction terms are \code{hdl.ltg}, \code{bmi.hdl}, \code{map.ltg} and \code{bmi.map}.  The model stability plot suggests that there are no dominant models of size 2, 3 or 4.  Furthermore there are no models of size 2, 3 or 4 that make large improvements in description loss. There is a dominant model of dimension 5 that is selected in 56\% of bootstrap resamples.  The variables selected in the dominant model are \{\code{bmi.map}, \code{bmi.hdl}, \code{map.ltg}, \code{hdl.ltg}\}, which can be found in the print output above.  Furthermore, this model does make a reasonable improvement in description loss, almost in line with the full model.  This finding is reinforced in the adaptive fence plots where there are only two regions of stability, one for the null model and another for the \{\code{bmi.map}, \code{bmi.hdl}, \code{map.ltg}, \code{hdl.ltg}\} model. In this instance, the difference between \code{best.only = TRUE} and \code{best.only = FALSE} is minor.


Hence, as a final model for the diabetes example we suggest including the main effects \{\code{bmi}, \code{map}, \code{ltg}, \code{sex}, \code{hdl}\} and the interaction effects \{\code{bmi.map}, \code{bmi.hdl}, \code{map.ltg}, \code{hdl.ltg}\}.  Further investigation can also be useful.  For example, we could use cross validation to compare the model with interaction effects, the model with just main effects and other simpler models that were identified as having peaks in the adaptive fence.  Researchers should also incorporate their specialist knowledge of the predictors and evaluate whether or not the estimated model is sensible from a scientific perspective.