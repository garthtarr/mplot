#' Model stability and variable importance plots for glmnet
#'
#'
#' @param mf a fitted 'full' model, the result of a call
#'   to lm or glm.
#' @param nlambda how many penalty values to consider.  Default = 100.
#' @param lambda manually specify the penalty values (optional).
#' @param B number of bootstrap replications
#' @param cores number of cores to be used when parallel
#'   processing the bootstrap (Not yet implemented.)
#' @param force.in the names of variables that should be forced
#'   into all estimated models. (Not yet implemented.)
#' @param penalty.factor Separate penalty factors can be applied to each
#'   coefficient. This is a number that multiplies lambda to allow
#'   differential shrinkage. Can be 0 for some variables, which implies
#'   no shrinkage, and that variable is always included in the model.
#'   Default is 1 for all variables (and implicitly infinity for variables
#'   listed in exclude). Note: the penalty factors are internally rescaled
#'   to sum to nvars, and the lambda sequence will reflect this change.
#' @param screen logical, whether or not to perform an initial
#'   screen for outliers.  Highly experimental, use at own risk.
#'   Default = FALSE.
#' @param redundant logical, whether or not to add a redundant
#'   variable.  Default = \code{TRUE}.
#' @param seed random seed for reproducible results
#' @param ... further arguments (currently unused)
#' @details The result of this function is essentially just a
#'   list. The supplied plot method provides a way to visualise the
#'   results.
#' @export
#' @seealso \code{\link{plot.bglmnet}}
#' @examples
#' n = 100
#' set.seed(11)
#' e = rnorm(n)
#' x1 = rnorm(n)
#' x2 = rnorm(n)
#' x3 = x1^2
#' x4 = x2^2
#' x5 = x1*x2
#' y = 1 + x1 + x2 + e
#' dat = data.frame(y, x1, x2, x3, x4, x5)
#' lm1 = lm(y ~ ., data = dat)
#' \dontrun{
#' bg1 = bglmnet(lm1, seed = 1)
#' plot(bg1)
#' plot(bg1, which = "boot_size", interactive = TRUE)
#' plot(bg1, which = "boot_size", interactive = FALSE)
#' plot(bg1, which = "boot", interactive = TRUE)
#' plot(bg1, which = "boot", interactive = FALSE)
#' plot(bg1, which = "vip", interactive = TRUE)
#' plot(bg1, which = "vip", interactive = FALSE)
#' }


bglmnet = function(mf, nlambda = 100, lambda = NULL, B = 100,
                   penalty.factor, screen = FALSE,
                   redundant = TRUE,
                   cores = NULL,
                   force.in = NULL,
                   seed = NULL) {
  
  if(!is.null(seed)) {
    set.seed(seed)
  }
  
  m = mextract(mf, screen = screen,
               redundant = redundant)
  
  fixed = m$fixed
  yname = m$yname
  X = m$X
  kf = m$k
  n = m$n
  family = m$family
  fam = family$family
  if (!is.element(fam,c("gaussian", "binomial", "poisson","multinomial", "cox", "mgaussian"))) {
    stop(paste("family is",fam,
               "but it needs to be one of gaussian, binomial, poisson, multinomial, cox, mgaussian"),
         call. = FALSE)
  }
  
  Xy = m$X
  kf = m$k
  X = Xy[,1:(kf - 1)]
  Y = Xy[,kf]
  n = m$n
  X = scale(X) * sqrt(n)/sqrt(n - 1)
  #X[which(is.na(X))] = 0
  X = cbind(1, X)
  colnames(X) = c("(Intercept)",colnames(X)[-1])
  if (missing(penalty.factor)) {
    # link this with force.in
    penalty.factor = c(0, rep(1, kf))
  }
  if (!is.null(lambda)) {
    nlambda = length(lambda)
  }
  temp = glmnet::glmnet(X, Y, alpha = 1, nlambda = nlambda,
                        lambda = lambda,
                        penalty.factor = penalty.factor,
                        weights = m$wts)
  mat = NULL
  # redefine lambda explicitly
  lambda = temp$lambda
  nlambda = length(lambda)
  compteur = matrix(0, kf, nlambda)
  mfstar = do.call("glm",list(fixed, data = Xy, family = family, weights = m$wts))
  #ystar = stats::simulate(object = mfstar, nsim = B)
  #ystar[is.na(ystar)] = Xy[is.na(ystar),yname]
  
  betaboot = array(0,dim = c(kf,nlambda,B))
  rownames(betaboot) = names(mfstar$coef)
  for (j in 1:B) {
    wts = stats::rexp(n = n, rate = 1) * m$wts
    for (i in 1:nlambda) {
      temp = glmnet::glmnet(X, Y, #ystar[,j], 
                            alpha = 1,
                            lambda = lambda[i],
                            intercept = TRUE,
                            #penalty.factor = penalty.factor,
                            family = fam,
                            weights = wts)
      betaboot[,i,j] = (temp$beta[, 1] != 0)
    }
  }
  # looking at model selection across bootstrap replications
  get_unique_mods = function(x) unique(t((x)))
  get.names = function(x) paste(names(x)[x == 1],collapse = "+")
  
  
  prob = NULL # hack for no visible binding for global variable 'prob'
  mod.sum = betaboot %>% 
    apply(3, get_unique_mods) %>% 
    do.call(rbind, .) %>% 
    data.frame() %>% 
    dplyr::mutate(k = rowSums(.)) %>% 
    dplyr::group_by_all() %>% 
    dplyr::summarise(n = n()) %>% 
    dplyr::arrange(k,n) %>% 
    dplyr::mutate(
      k = k + 1
    ) %>% 
    dplyr::group_by(k) %>% 
    dplyr::mutate(
      prob = n/sum(n)
    ) %>% dplyr::ungroup()
  mod.sum$mod.names = mod.sum %>% dplyr::select(-k,-n,-prob) %>% apply(.,1,get.names)
  mod.sum$mod.names[mod.sum$mod.names == ""] = "1"
  mod.sum$mod.names = gsub(pattern = "X.Intercept.",
                           replacement = "1", x = mod.sum$mod.names)
  mod.sum$logLikelihood = NA
  
  for (i in 1:nrow(mod.sum)) {
    # don't need to do this for models that include REDUNDANT.VARIABLE
    mod.sum$logLikelihood[i] = stats::logLik(stats::glm(stats::as.formula(paste(yname,"~",mod.sum$mod.names[i])),
                                                        data = Xy,
                                                        family = family,
                                                        weights = m$wts))
  }
  
  # looking at variable inclusion across lambda values
  compteur2 = apply(betaboot, c(1,2), sum)
  probavariable = compteur2/B
  colnames(probavariable) = round(lambda,3)
  mods = list()
  for (k in 1:length(lambda)) {
    mods[[k]] = table(apply(betaboot[,k,],2,get.names))
  }
  # all.mods = unique(names(unlist(mods)))
  # all.mods[all.mods == ""] = "1"
  # all.ll = rep(0,length(all.mods))
  # all.k = rep(0,length(all.mods))
  # for (k in 1:length(all.mods)) {
  #   # don't need to do this for models that include REDUNDANT.VARIABLE
  #   all.ll[k] = -2*stats::logLik(stats::glm(stats::as.formula(paste(yname,"~",all.mods[k])),
  #                                           data = Xy,
  #                                           family = family,
  #                                           weights = m$wts))
  #   # number of variables including intercept
  #   all.k[k] = length(unlist(strsplit(all.mods[[k]],
  #                                     split = "+",fixed = TRUE))) + 1
  # }
  # all.k[all.mods=="1"] = 1
  # mod.sum2 = data.frame(mod.names = all.mods, ll=all.ll, k=all.k)
  blarout = list(frequency = probavariable,
                 lambda = lambda,
                 mods = mods,
                 mod.sum = mod.sum,
                 screen = screen,
                 vars = names(mfstar$coef),
                 call = match.call())
  class(blarout) = "bglmnet"
  return(blarout)
}


#' Plot diagnostics for a bglmnet object
#'
#' A plot method to visualise the results of a \code{bglmnet} object.
#'
#' @param x \code{bglmnet} object, the result of \code{\link{bglmnet}}
#' @param highlight the name of a variable that will be highlighted.
#' @param interactive logical.  If \code{interactive=TRUE} a
#'   googleVis plot is provided instead of the base graphics plot.
#'   Default is \code{interactive=FALSE}.
#' @param classic logical.  Depricated. If \code{classic=TRUE} a
#'   base graphics plot is provided instead of a googleVis plot. 
#'   For now specifying \code{classic} will overwrite the 
#'   default \code{interactive} behaviour, though this is
#'   likely to be removed in the future.
#' @param tag Default NULL. Name tag of the objects to be extracted 
#' from a gvis (googleVis) object. 
#' 
#' The default tag for is NULL, which will 
#' result in R opening a browser window.  Setting \code{tag='chart'} 
#' or setting \code{options(gvis.plot.tag='chart')} is useful when 
#' googleVis is used in scripts, like knitr or rmarkdown. 
#' 
#' @param shiny Default FALSE. Set to TRUE when using in a shiny interface.
#' 
#' @param which a vector specifying the plots to be output. Variable
#'   inclusion type plots \code{which="vip"} or model description loss against
#'   penalty parameter \code{which="boot"}.
#' @param width Width of the googleVis chart canvas area, in pixels.
#'   Default: 800.
#' @param height Height of the googleVis chart canvas area, in pixels.
#'   Default: 400.
#' @param chartWidth googleVis chart area width.
#'   A simple number is a value in pixels;
#'   a string containing a number followed by \code{\%} is a percentage.
#'   Default: \code{"60\%"}
#' @param chartHeight googleVis chart area height.
#'   A simple number is a value in pixels;
#'   a string containing a number followed by \code{\%} is a percentage.
#'   Default: \code{"80\%"}
#' @param fontSize font size used in googleVis chart.  Default: 12.
#' @param left space at left of chart (pixels?).  Default: "50".
#' @param top space at top of chart (pixels?).  Default: "30".
#' @param axisTitlesPosition Where to place the googleVis axis titles,
#'   compared to the chart area. Supported values:
#'   "in" - Draw the axis titles inside the the chart area.
#'   "out" - Draw the axis titles outside the chart area.
#'   "none" - Omit the axis titles.
#' @param text logical, whether or not to add text labels to classic
#'   boot plot. Default = \code{FALSE}.
#' @param dataOpacity The transparency of googleVis data points,
#'   with 1.0 being completely opaque and 0.0 fully transparent.
#' @param options a list to be passed to the googleVis function giving
#'   complete control over the output.  Specifying a value for
#'   \code{options} overwrites all other plotting variables.
#' @param backgroundColor The background colour for the main area
#'   of the chart. A simple HTML color string,
#'   for example: 'red' or '#00cc00'.  Default: 'transparent'
#' @param min.prob lower bound on the probability of a model being selected. If
#'   a model has a selection probability lower than \code{min.prob} it will not be
#'   plotted.
#' @param jitterk amount of jittering of the model size in the lvk and boot plots.
#'   Default = 0.1.
#' @param legend.position the postion of the legend for classic plots.
#'   Default \code{legend.position="right"} alternatives include
#'   \code{legend.position="top"} and \code{legend.position="bottom"}
#' @param max.circle  determines the maximum circle size.
#'   Default = 15.
#' @param srt when \code{text=TRUE}, the angle of rotation for the text labels.
#'   Default = 45.
#' @param ylim the y limits of the \code{which="boot"} plots.
#' @param hAxis.logScale logical, whether or not to use a log scale on
#'   the horizontal axis. Default = TRUE.
#' @param ... further arguments (currently unused)
#' @export
#' @seealso \code{\link{bglmnet}}

plot.bglmnet = function(x, highlight, interactive = FALSE, 
                        classic = NULL, 
                        tag = NULL, shiny = FALSE,
                        which=c("boot","vip","lvk"),
                        width=800, height=400, fontSize=12,
                        left=50, top=30,
                        chartWidth="60%",
                        chartHeight="80%",
                        axisTitlesPosition="out",
                        dataOpacity=0.5,
                        options=NULL,
                        hAxis.logScale = TRUE,
                        ylim, text = FALSE,
                        backgroundColor = 'transparent',
                        legend.position = "right",
                        jitterk = 0.1,
                        srt = 45,
                        max.circle = 15,
                        min.prob = 0.1, ...) {
  if (!is.null(classic)) interactive = !classic
  if (backgroundColor == "transparent") {
    backgroundColor = "{stroke:null, fill:'null', strokeSize: 0}"
  } else {
    backgroundColor = paste("{stroke:null, fill:'",backgroundColor,
                            "', strokeSize: 0}", sep = "")
  }
  B = sum(x$mods[[1]])
  gvis.hAxis = paste("{title:'Penalty parameter',
                    logScale:'",hAxis.logScale,"' ,
                    baseline:",0," ,
                     maxValue:",max(x$lambda)*1.1," ,
                     minValue:",min(x$lambda),"}",sep="")
  
  
  if (base::missing(highlight)) {
    no.highlight = TRUE
    highlight =  x$vars[2]
    # if(sum(df.sub$k==2)>0){ # highlight best bivariate variable
    #   dfk2 = unique(df.sub[df.sub$k==2,c(1,5)])
    #   highlight = dfk2$mod.names[which.min(dfk2$ll)]
    # } else highlight =  x$vars[2]
  }
  
  reverselog_trans <- function(base = 10) {
    trans <- function(x) -base::log(x, base)
    inv <- function(x) base^(-x)
    scales::trans_new(base::paste0("reverselog-", base::format(base)), 
                      trans, inv, 
                      scales::log_breaks(base = base), 
                      domain = c(1e-100, Inf))
  }
  
  
  if("boot" %in% which){
    
    l.vec = rep(x$lambda, times = lapply(x$mods,length))
    mod.vec = unlist(x$mods)
    mod.names = names(mod.vec)
    mod.names[mod.names==""] = "1"
    mod.vec.counts = as.numeric(mod.vec)
    mod.vec.prob = mod.vec.counts/B
    df.temp = data.frame(l.vec,mod.vec.counts,mod.vec.prob,mod.names)
    # remove redundant variables
    df = df.temp[-grep("REDUNDANT.VARIABLE",df.temp$mod.names),]
    df.full = merge(df,x$mod.sum,all.x = TRUE)
    if(all(df.full$mod.vec.prob<min.prob))
      min.prob = stats::quantile(df.full$mod.vec.prob,0.75)
    df.sub = subset(df.full, df.full$mod.vec.prob > min.prob)
    df.sub$mod.names = as.character(df.sub$mod.names)
    
    
    mod.parts = lapply(df.sub$mod.names,FUN = strsplit,"+",fixed=TRUE)
    find.var = function(x,highlight){
      is.element(highlight,unlist(x))
    }
    var.ident = unlist(lapply(mod.parts, find.var,highlight=highlight))
    var.ident[var.ident==TRUE] =  paste("With",highlight)
    var.ident[var.ident==FALSE] =  paste("Without",highlight)
    df.sub$var.ident = var.ident
    df.sub$m2ll = -2*df.sub$logLikelihood
    
    if (! interactive) {
      
      p = ggplot2::ggplot(
        data = df.sub,
        ggplot2::aes_string(x = "l.vec", 
                            y = "m2ll",
                            label = "mod.names")) + 
        ggplot2::geom_jitter(
          ggplot2::aes_string(size = "mod.vec.prob",
                              fill = "var.ident"),
          shape = 21,
          width = 0.0,
          alpha = 0.4) + 
        ggplot2::scale_x_continuous(trans = reverselog_trans()) +
        ggplot2::theme_bw(base_size = 14) + 
        ggplot2::labs(y = "-2*Log-likelihood",
                      x = "Penalty parameter") + 
        ggplot2::theme(legend.title = ggplot2::element_blank(),
                       legend.key = ggplot2::element_blank(),
                       legend.position = "right") +
        ggplot2::scale_fill_manual(values = ggplot2::alpha(c("red","blue"), .4)) +
        ggplot2::guides(
          fill = ggplot2::guide_legend(
            override.aes = list(
              shape = 22,
              size = 5,
              fill = ggplot2::alpha(c("red","blue"), .4)
            )
          )
        )
      
      # if (!missing(ylim)) 
      #   p = p + ggplot2::ylim(ylim[1],ylim[2])
      
      return(p)
      
    } else { # interactive = TRUE
      
      gvis.title = paste("Model stability plot for glmnet",sep="")
      #x.ticks=paste(1:max(x$lk$k),collapse=",")
      chartArea = paste("{left:",left,
                        ",top:",top,
                        ",width:'",chartWidth,
                        "',height:'",chartHeight,"'}",sep="")
      bubble = paste("{opacity:",dataOpacity,
                     ", textStyle: {color: 'none'}}",sep="")
      
      if(is.null(options)){
        use.options=list(title=gvis.title,
                         fontSize = fontSize,
                         vAxis="{title:'-2*Log-likelihood'}",
                         hAxis=gvis.hAxis,
                         sizeAxis = "{minValue: 0, minSize: 1,
                         maxSize: 20, maxValue:1}",
                         axisTitlesPosition=axisTitlesPosition,
                         bubble = bubble,
                         chartArea=chartArea,
                         width=width, height=height,
                         backgroundColor=backgroundColor,
                         explorer= "{axis: 'vertical',
                         keepInBounds: true,
                         maxZoomOut: 1,
                         maxZoomIn: 0.01,
                         actions: ['dragToZoom',
                         'rightClickToReset']}")
      } else {use.options = options}
      
      fplot = googleVis::gvisBubbleChart(data = df.sub,
                                         idvar = "mod.names",
                                         xvar = "l.vec",
                                         yvar = "m2ll", 
                                         colorvar = "var.ident",
                                         sizevar = "mod.vec.prob",
                                         options = use.options)
      if(shiny){
        return(fplot)
      } else {
        graphics::plot(fplot, tag = tag)
      }
    }
  }
  
  if("boot_size" %in% which) {
    
    
    pd = x$mod.sum
    pd$var.ident.tf = NA
    pd$var.ident.tf = pd[, highlight[1]] == 1
    #vi = var.ident
    pd$var.ident = NA
    pd$var.ident[pd$var.ident.tf == TRUE] = paste("With", highlight[1])
    pd$var.ident[pd$var.ident.tf == FALSE] = paste("Without", highlight[1])
    pd$m2ll = -2*pd$logLikelihood
    
    if(!interactive){
      
      p = ggplot2::ggplot(
        pd,
        ggplot2::aes_string(
          x = "k",
          y = "m2ll",
          group = "var.ident",
          label = "mod.names"
        )) +
        ggplot2::geom_jitter(
          ggplot2::aes_string(size = "prob",
                              fill = "var.ident"),
          shape = 21,
          width = jitterk
        ) +
        ggplot2::scale_size(range = c(0, max.circle)) +
        ggplot2::theme_bw(base_size = 14) +
        ggplot2::ylab("-2*Log-likelihood") +
        ggplot2::xlab("Number of parameters") +
        ggplot2::theme(
          legend.title = ggplot2::element_blank(),
          legend.key = ggplot2::element_blank(),
          legend.position = legend.position
        ) +
        ggplot2::scale_fill_manual(values = ggplot2::alpha(c("red", "blue"), .4)) +
        ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(
          shape = 22,
          size = 5,
          fill = ggplot2::alpha(c("red", "blue"), .4)
        )))
      if (!base::missing(ylim))
        p = p + ggplot2::ylim(ylim[1], ylim[2])
      if (text) {
        p = p + ggplot2::geom_text(hjust = 0, angle = srt)
      }
      return(p)  
      
    } else { # interactive = TRUE
      gvis.title = paste("Model stability plot (lasso)", sep = "")
      x.ticks = paste(1:max(pd$k), collapse = ",")
      gvis.hAxis = paste(
        "{title:'Number of parameters',
        maxValue:",
        max(pd$k) + 0.5,
        " ,
        minValue:",
        0.5,
        " ,
        ticks: [",
        x.ticks,
        "]}"
      )
      y.min = min(pd$m2ll) - 0.02*(max(pd$m2ll)- min(pd$m2ll))
      gvis.vAxis = paste("{title:'-2*Log-likelihood', minValue:",
                         y.min, "}")
      chartArea = paste(
        "{left:", left, ",top:", top,
        ",width:'", chartWidth, "',height:'", chartHeight,
        "'}", sep = ""
      )
      bubble = paste("{opacity:",
                     dataOpacity,
                     ", textStyle: {color: 'none'}}",
                     sep = "")
      if (is.null(options)) {
        use.options = list(
          title = gvis.title,
          fontSize = fontSize,
          vAxis = gvis.vAxis,
          hAxis = gvis.hAxis,
          sizeAxis = "{minValue: 0, minSize: 1,
          maxSize: 20, maxValue:1}",
          axisTitlesPosition = axisTitlesPosition,
          bubble = bubble,
          chartArea = chartArea,
          width = width,
          height = height,
          backgroundColor = backgroundColor,
          explorer = "{axis: 'vertical',
          keepInBounds: true,
          maxZoomOut: 1,
          maxZoomIn: 0.01,
          actions: ['dragToZoom',
          'rightClickToReset']}"
        )
      } else {
        use.options = options
      }
      fplot = googleVis::gvisBubbleChart(
        data = pd,
        idvar = "mod.names",
        xvar = "k",
        yvar = "m2ll",
        colorvar = "var.ident",
        sizevar = "prob",
        options = use.options
      )
      if (shiny) {
        return(fplot)
      } else {
        graphics::plot(fplot, tag = tag)
      }
    }
  }
  
  if ("vip" %in% which) {
    
    var.names = x$vars[x$vars != "(Intercept)"]
    p.var = t(x$freq)
    p.var = p.var[,colnames(p.var) %in% var.names]
    sortnames = names(sort(apply(p.var, 2, mean), decreasing = TRUE))
    vip.df = p.var[,sortnames]
    rownames(vip.df) = NULL
    vip.df = data.frame(lambda = x$lambda, vip.df)
    #tid = c(1,2,4,6:dim(vip.df)[2])
    #vip.df[, tid] = sapply(vip.df[, tid], as.numeric)
    colnames(vip.df) = gsub("REDUNDANT.VARIABLE", "RV", colnames(vip.df))
    sortnames = gsub("REDUNDANT.VARIABLE", "RV", sortnames)
    if(!interactive) { 
      lambda = NULL # hack for no visible binding for global variable 'lambda'
      ggdat = vip.df %>% 
        tidyr::gather(-lambda, key = "variable", value = "prob")
      p = ggplot2::ggplot(
        data = ggdat,
        ggplot2::aes_string(
          x = "lambda",
          y = "prob",
          colour = "variable"
        )
      ) +
        ggplot2::geom_line() +
        ggplot2::theme_bw(base_size = 14) +
        ggplot2::labs(y = "Bootstrap inclusion probability",
                      x = "Penalty") +
        ggplot2::scale_x_log10() +
        ggplot2::theme(
          legend.title = ggplot2::element_blank(),
          legend.key = ggplot2::element_blank(),
          legend.position = legend.position
        )
      
      return(p)
      
    } else { # interactive = TRUE
      
      gvis.title = "Variable inclusion plot (lasso)"
      chartArea = paste("{left:",left,
                        ",top:",top,
                        ",width:'",chartWidth,
                        "',height:'",chartHeight,"'}", sep = "")
      if (is.null(options)) {
        use.options = list(title = gvis.title,
                           fontSize = fontSize,
                           vAxis = "{title:'Bootstrapped probability'}",
                           hAxis = gvis.hAxis,
                           sizeAxis = "{minValue: 0, minSize: 1,
                           maxSize: 20, maxValue:1}",
                           axisTitlesPosition = axisTitlesPosition,
                           chartArea = chartArea,
                           width = width, height = height,
                           backgroundColor = backgroundColor,
                           annotations = "{style:'line'}")
      } else {use.options = options}
      fplot = googleVis::gvisLineChart(data = vip.df,
                                       xvar = "lambda",
                                       yvar = sortnames,
                                       options = use.options)
      if(shiny){
        return(fplot)
      } else {
        return(graphics::plot(fplot, tag = tag))
      }
    }
  } else return(invisible())
  
}
