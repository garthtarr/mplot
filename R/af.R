#' The adaptive fence procedure
#' 
#' This function implements the adaptive fence procedure to
#' first find the optimal cstar value and then finds the 
#' corresponding best model as described in Jiang et. al.
#' (2009) with some practical modifications.
#' 
#' The initial stepwise procedure performs forward stepwise model
#' selection using the AIC and backward stepwise model selection 
#' using BIC.  In general the backwise selection via the more 
#' conservative BIC will tend to select a smaller model than that
#' of the forward selection AIC approach.  The size of these two
#' models is found, and we go two dimensions smaller and larger
#' to estiamte a sensible range of \code{c} values over which to
#' perform a parametric bootstrap.
#' 
#' This procedure can take some time.  It is recommended that you start
#' with a relatively small number of bootstrap samples (\code{B}) 
#' and grid of boundary values (\code{n.c}) and increase both as 
#' required.
#' 
#' If you use \code{initial.stepwise=TRUE} then in general you will 
#' need a smaller grid of boundary values than if you select 
#' \code{initial.stepwise=FALSE}.
#' It can be useful to check \code{initial.stepwise=FALSE} with a
#' small number of bootstrap replications over a sparse grid to ensure
#' that the \code{initial.stepwise=TRUE} has landed you in a reasonable 
#' region.
#' 
#' 
#' @param mf a fitted 'full' model, the result of a call
#'   to lm or glm (and in the future lme or lmer).
#' @param B number of bootstrap replications at each fence
#'   boundary value
#' @param n.c number of boundary values to be considered
#' @param initial.stepwise logical.  Performs an initial stepwise 
#'   procedure to look for the range of model sizes where attention
#'   should be focussed. See details for implementation.
#' @param force.in the names of variables that should be forced
#'   into all estimated models.
#' @param n.cores number of cores to be used when parallel
#'   processing the bootstrap.
#' @param nvmax size of the largest model that can still be 
#'   considered as a viable candidate.  Included for performance
#'   reasons but if it is an active constraint it could lead to
#'   missleading results.
#' @param c.max manually specify the upper boundary limit. 
#'   Only applies when \code{initial.stepwise=FALSE}.
#' @param ... further arguments (currently unused)
#' @seealso \code{\link{lmfence}}, \code{\link{glmfence}}
#' @references Jiming Jiang, Thuan Nguyen, J. Sunil Rao, 
#'   A simplified adaptive fence procedure, Statistics & 
#'   Probability Letters, Volume 79, Issue 5, 1 March 2009, 
#'   Pages 625-629, http://dx.doi.org/10.1016/j.spl.2008.10.014.
#' @export
#' @family fence
#' @examples
#' n = 100
#' set.seed(11)
#' e = rnorm(n)
#' x1 = rnorm(n)
#' x2 = rnorm(n)
#' x3 = x1^2
#' x4 = x2^2
#' x5 = x1*x2
#' x6 = rep(c("A","B"),n=50)
#' y = 1 + x1 + x2 + e
#' dat = data.frame(y,x1,x2,x3,x4,x5,x6)
#' lm1 = lm(y~.,data=dat)
#' af1 = af(lm1, n.cores=4, initial.stepwise=TRUE)
#' summary(af1)
#' \dontrun{
#' plot(af1)
#' }

af = function(mf,
              B=60, n.c=20,
              initial.stepwise=FALSE,
              force.in=NULL, 
              n.cores, nvmax, c.max, ...){
  method="ML"
  af.call = match.call()
  if(!missing(c.max) & initial.stepwise==TRUE) {
    initial.stepwise=FALSE
    warning("When c.max is specified, initial.stepwise=FALSE")
  }
  if(!any(class(mf)=="lm")){
    warning("Adaptive fence currently only implemented for lm and glm")
    model.type = "lm"
  }
  if(any(class(mf)=="glm")==TRUE){
    family=family(mf)
    if(!is.null(force.in)){
      warning("force.in is not implemented for glms")
    }
    model.type="glm"
  } else if(class(mf)=="lm"){
    model.type="lm"
  }
  m = mextract(mf) 
  fixed = m$fixed
  yname = m$yname
  family = m$family
  Xy = m$X
  kf = m$k
  n = m$n
  null.ff = as.formula(paste(yname,"~1"))
  #REDUNDANT.VARIABLE = rnorm(n)
  # full model plus redundant variable
  #Xy = data.frame(Xy[!names(Xy)%in%yname],
  #                   REDUNDANT.VARIABLE,
  #                   Xy[names(Xy)%in%yname]) 
  #fixed = as.formula(paste(paste(deparse(fixed),collapse=''),
  #                            "+REDUNDANT.VARIABLE"))
  if(model.type=="glm"){
    m0 = glm(null.ff, data = Xy, family=family) 
    mfstar = glm(fixed, data = Xy, family=family) 
  } else {
    m0 = lm(null.ff, data = Xy) 
    mfstar = lm(fixed, data = Xy) 
  }
  Qm0 = Qm(m0, method=method)
  Qmfstar = Qm(mfstar, method=method)
  if(!is.null(force.in)){
    small.ff = as.formula(paste(yname,"~",paste(force.in,collapse="+")))
  } else {
    small.ff = null.ff
  }
  if (initial.stepwise) {
    if(model.type=="glm"){
      small.mod = glm(small.ff, data = Xy, family=family)
    } else {
      small.mod = lm(small.ff, data = Xy)
    }
    # backwards and forwards model selection using
    # BIC (conservative) and AIC (less conservative)
    bwds.BIC = step(mfstar, 
                    scope = list(lower=small.ff, upper=fixed),
                    direction="backward", k=log(n), trace=0)
    fwds.BIC = step(small.mod, 
                    scope = list(lower=small.ff, upper=fixed),
                    direction="forward", k=log(n), trace=0)
    k.min = max(min(length(bwds.BIC$coef),length(fwds.BIC$coef))-2,1)
    bwds.AIC = step(mfstar, 
                    scope = list(lower=small.ff, upper=fixed),
                    direction="backward", k=2, trace=0)
    fwds.AIC = step(small.mod, 
                    scope = list(lower=small.ff, upper=fixed),
                    direction="forward", k=2, trace=0)
    k.max = min(max(length(bwds.AIC$coef),length(fwds.AIC$coef))+2,kf)  
    k.range = list(k.min=k.min,k.max=k.max)
    Q.range = qrange(k.range=k.range, data=Xy, 
                     yname=yname, fixed=fixed, 
                     method=method, force.in=force.in,
                     model.type = model.type,
                     family = family)
    c.max = (Q.range$Q.max - Qmfstar)*1.1
    c.min = max(Q.range$Q.min - Qmfstar,0)*0.9
    c.range = seq(c.min,c.max,length.out=n.c)
    if(missing(nvmax)) nvmax = k.max
  } else {
    k.range = list(k.min=1,k.max=kf)
    if(missing(c.max)) c.max = (Qm0-Qmfstar)*1.1
    c.min = 0.1
    c.range = seq(c.min,c.max,length.out=n.c)
    if(missing(nvmax)) nvmax = kf
  }
  
  require(doParallel)
  if(missing(n.cores)) n.cores = max(detectCores()-1,1)
  cl.af = makeCluster(n.cores)
  registerDoParallel(cl.af)
  require(foreach)
  p.star.all = foreach(j = 1:n.c, .combine=rbind, .packages = c("mplot")) %dopar% {
    fence.mod = list()
    fence.rank = list()
    ystar = simulate(object=mfstar,nsim=B)
    if(model.type=="glm"){
      for(i in 1:B){
        Xy[yname] = ystar[,i]
        mfstarB = glm(fixed,data=Xy,family=family)
        fms = glmfence(mfstarB, cstar=c.range[j], 
                       trace=FALSE, nvmax=nvmax, adaptive=TRUE)
        fence.mod = c(fence.mod,fms)
        fence.rank = c(fence.rank,1:length(fms))
      } 
    } else {
      for(i in 1:B){
        Xy[yname] = ystar[,i]
        mfstarB = lm(fixed,data=Xy)
        fms = lmfence(mfstarB, cstar=c.range[j], trace=FALSE,
                      nvmax = nvmax, force.in=force.in, adaptive=TRUE)
        fence.mod = c(fence.mod,fms)
        fence.rank = c(fence.rank,1:length(fms))
      } 
    }
    process.fn(fence.mod,fence.rank)
  }
  stopCluster(cl.af)
  
  # Another function that processes results within af function
  # 
  # This function is used by the af function to process
  # the results when iterating over different boundary values
  
  pstar.fn = function(input,type){
    if(type=="bo"){
      p.star = input[,1:2]
    } else if(type=="all"){
      p.star = input[,3:4]
    }
    pstarmods = sort(table(p.star[,2]),decreasing=TRUE)
    n.pstarmods = length(pstarmods)
    p.star = data.frame(pstar = as.numeric(p.star[,1]),
                        model = as.character(p.star[,2]),
                        modelident = match(p.star[,2], names(pstarmods)))
    redundent.vars = grepl("REDUNDANT.VARIABLE",as.character(p.star$model))
    c.range = c.range[!redundent.vars]
    p.star = p.star[!redundent.vars,]
    p.star$model = droplevels(p.star$model)
    # if want runs of (near) maximums
    max.p = max(p.star[,1]) #- 2/B
    tf = p.star[,1] >= max.p
    a = rle(tf)
    pos = which(a$values==TRUE)
    # what if there was a sequence of falses of the same length?
    # keep only the position of these runs where we had true values
    pos = pos[a$values[pos]==TRUE]
    mid = NA
    for(i in 1:length(pos)){
      # find the midpoint
      if(pos[i]==1){
        mid[i] = sum(a$lengths[1:pos[i]]+1)/2
      } else {
        mid[i] = (sum(a$length[1:pos[i]]) + sum(a$lengths[1:(pos[i]-1)])+1)/2
      }
    }
    mid = floor(mid)
    c.star = min(c.range[mid])
    if(model.type=="glm"){
      afmod = glmfence(mf, cstar=c.star, 
                       trace=FALSE, nvmax=nvmax)[[1]]
    } else {
      afmod = lmfence(mf, cstar=c.star, trace=FALSE,
                      nvmax=nvmax, force.in=force.in)[[1]]
    }
    p.star[,1] = as.numeric(as.character(p.star[,1]))
    return(list(p.star=p.star,
                c.range=c.range,
                c.star=c.star,
                model = afmod))
  }
  # set up the output object class
  afout = list()
  afout$bestOnly = pstar.fn(p.star.all,type="bo")
  afout$all = pstar.fn(p.star.all,type="all")
  afout$call = af.call
  if(initial.stepwise){
    afout$initial.stepwise = list(
      fwds.AIC = as.formula(fwds.AIC),
      fwds.BIC = as.formula(fwds.BIC),
      bwds.AIC = as.formula(bwds.AIC),
      bwds.BIC = as.formula(bwds.BIC))
  } else {afout$initial.stepwise=NULL}
  afout$k.range = k.range
  class(afout) = "af"
  return(afout)
}

#' Summary method for an af object
#' 
#' Provides comprehensive  output of the bootstrap results of an 
#' af object.
#' 
#' @param object \code{af} object, the result of \code{\link{af}}
#' @param best.only logical determining whether the output used the
#'   standard fence approach of only considering the best models
#'   that pass the fence (\code{TRUE}) or if it should take into 
#'   account all models that pass the fence at each boundary 
#'   value (\code{FALSE}).
#' @param ... further arguments (currently unused)
#' @export
# S3 method for class 'af'
summary.af = function (object,best.only=TRUE,...) {
  if(best.only){
    xsub = object$bestOnly
  } else {
    xsub = object$all
  }
  cat("\nCall:\n", paste(deparse(object$call), sep = "\n", collapse = "\n"), 
      "\n\n", sep = "")
  cat("Adaptive fence model (c*=")
  cat(round(xsub$c.star,1))
  cat("):\n")
  cat(deparse(xsub$model))
  cat("\n\n")
  cat("Model sizes considered: ")
  cat(object$k.range$k.min)
  cat(" to ")
  cat(object$k.range$k.max)
  cat(" (including intercept).")
  cat("\n\n")
  if(!is.null(object$initial.stepwise)){
    cat("Stepwise procedures:\n")
    cat("Forwards AIC: ")
    cat(deparse(object$initial.stepwise$fwds.AIC))
    cat("\n")
    cat("Backwards AIC: ")
    cat(deparse(object$initial.stepwise$bwds.AIC))
    cat("\n")
    cat("Forwards BIC: ")
    cat(deparse(object$initial.stepwise$fwds.BIC))
    cat("\n")
    cat("Backwards BIC: ")
    cat(deparse(object$initial.stepwise$bwds.BIC))
    cat("\n\n")
  }
  invisible(object)
}


#' Plot diagnostics for an af object
#' 
#' Summary plot of the bootstrap results of an 
#' af object.
#' 
#' @param x \code{af} object, the result of \code{\link{af}}
#' @param classic logical.  If \code{classic=TRUE} a 
#'   base graphics plot is provided instead of a googleVis plot.
#'   Default is \code{classic=FALSE}.
#' @param best.only logical.  A modification to the adaptive
#'   procedure which considers all models at a particular
#'   size that pass the fence hurdle when calculating the
#'   p* values.  The rationale being that if a model is
#'   'correct' then it will be the only model at that size
#'   that passes the fence.  This can help to determine where
#'   the correct peak is to select c*.
#' @param pch plotting character, i.e., symbol to use.
#' @param html.only logical was be used with rmarkdown, though perhaps
#'   using \code{op = options(gvis.plot.tag = "chart")} is better.
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
#' @param options If you want to specify the full set of googleVis 
#'   options.
#' @param backgroundColor The background colour for the main area 
#'   of the chart. A simple HTML color string, 
#'   for example: 'red' or '#00cc00'.  Default: 'transparent'
#' @param ... further arguments (currently unused)
#' @export
# S3 method for class 'af'
plot.af = function(x,pch,classic=FALSE,
                   html.only=FALSE,
                   best.only=TRUE,
                   width=800,height=400,fontSize=12,
                   left=50,top=30,chartWidth="60%",
                   chartHeight="80%",
                   backgroundColor = 'transparent',
                   options=NULL,...){
  if(best.only){
    x = x$bestOnly
  } else {
    x = x$all
  }
  if(classic){
    if(missing(pch)) pch=19
    plot(x$p.star[,1]~x$c.range,
         ylim=c(0,1), pch=pch,
         col=x$p.star[,3],
         ylab = "p*", xlab = "c")
    legend("bottomleft",legend=unique(x$p.star[,2]),
           pch=pch,col=unique(x$p.star[,3]),bty="n")
    axis(side=3, at=x$c.star, 
         labels=paste("c*=", round(x$c.star,1),sep=""))
  } else {
    dat <- matrix(NA, nrow = nrow(x$p.star), 
                  ncol = nlevels(x$p.star$model) + 1)
    for(i in 1:nlevels(x$p.star$model)){
      lvl <- levels(x$p.star$model)[i]
      ind <- which(x$p.star$model == lvl)
      dat[ind, c(1, i+1)] <- x$p.star$pstar[ind]
    }
    plot.dat = data.frame(c.range = as.numeric(x$c.range), 
                          dat[,-1])
    colnames(plot.dat) = c("c.range",levels(x$p.star$model))
    plot.dat = round(plot.dat,2)
    # FOR FUN ON A RAINY DAY
    # INCLUDE ANNOTATION USING `ROLES'
    # SEE HERE: http://cran.r-project.org/web/packages/
    # googleVis/vignettes/Using_Roles_via_googleVis.html
    gvis.title = paste("Adaptive fence: c*=",round(x$c.star,1),sep="")
    namefunc <- function(v1) {
      deparse(substitute(v1))
    }
    chartArea = paste("{left:",left,
                      ",top:",top,
                      ",width:'",chartWidth,
                      "',height:'",chartHeight,"'}",sep="")
    if(is.null(options)){
      options=list(title=gvis.title,
                   fontSize=fontSize,
                   vAxis="{title:'p*',minValue:0,maxValue:1,
                           ticks: [0.0,0.2,0.4,0.6,0.8,1.0]}",
                   hAxis="{title:'c'}",
                   axisTitlesPosition="out",
                   chartArea=chartArea,
                   backgroundColor=backgroundColor,
                   width=width, height=height)
    }
    fplot = gvisScatterChart(data=plot.dat,options=options)
    if(html.only){
      return(fplot)
    } else {
      return(plot(fplot))
    }
  }
}


#' Print method for an af object
#' 
#' Prints basic output of the bootstrap results of an 
#' af object.
#' 
#' @param x an \code{af} object, the result of \code{\link{af}}
#' @param best.only logical.  A modification to the adaptive
#'   procedure which considers all models at a particular
#'   size that pass the fence hurdle when calculating the
#'   p* values.  The rationale being that if a model is
#'   'correct' then it will be the only model at that size
#'   that passes the fence.  This can help to determine where
#'   the correct peak is to select c*.
#' @param ... further arguments (currently unused)
#' @export
# S3 print method for class 'af'
print.af = function (x, best.only=TRUE, ...) {
  if(best.only){
    x = x$bestOnly
  } else {
    x = x$all
  }
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), 
      "\n\n", sep = "")
  cat("Adaptive fence model (c*=")
  cat(round(x$c.star,1))
  cat("):\n")
  cat(deparse(x$model))
  cat("\n\n")
  invisible(x)
}

