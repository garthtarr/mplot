#' The adaptive fence procedure (currently only for linear models)
#' 
#' This function implements the adaptive fence procedure to
#' first find the optimal cstar value and then finds the 
#' corresponding best (linear) model as described in Jiang et. al.
#' (2009) with some modifications.
#' 
#' The initial stepwise procedure performs forward stepwise model
#' selection using the AIC and backward stepwise model selection 
#' using BIC.  In general the backwise selection via the more 
#' conservative BIC will tend to select a smaller model than that
#' of the forward selection AIC approach.  The size of these two
#' models is found, and we go one dimension smaller and one dimension
#' bigger to estiamte a sensible range of c values over which to
#' perform a parametric bootstrap.  THIS DESCRIPTION IS VERY VAUGE
#' POSSIBLY NEEDS MORE DETAIL/MORE THOUGH ON HOW TO EXPRESS THE
#' PROCEDURE SUCCINCTLY.
#' 
#' This procedure can take some time.  It is recommended that you start
#' with a relatively small number of bootstrap samples and grid of c 
#' values and increase both if required.
#' 
#' If you use \code{initial.stepwise=TRUE} you will need a smaller 
#' grid of c values than if you select \code{initial.stepwise=FALSE}.
#' It can be interesting to check \code{initial.stepwise=FALSE} with a
#' small number of bootstrap replications over a sparse grid to ensure
#' that the \code{initial.stepwise=TRUE} has landed you in a reasonable 
#' region.
#' 
#' 
#' 
#' @param fixed an object of class \code{\link[stats]{formula}}
#'   specifying the full model that can be fitted.
#' @param random (not yet implemented) an object of 
#'   class \code{\link[stats]{formula}}
#'   specifying the random effect components.
#' @param family specify the family for a glm model
#' @param data a data frame containing the variables in the full
#'   model.  Currently this is required.  Future development will
#'   look at the \code{lm} function to replicate functionality,
#'   whereby if \code{data} is missing or variables are not found 
#'   in \code{data}, the variables are taken from 
#'   \code{environment(formula)}, typically the
#'   environment from which \code{lmfence} is called.
#' @param method the model selection method to be used. Currently
#'   only \code{method = "ML"} is supported (perhaps in the future
#'   \code{method = "MVC"} will be implemented).
#' @param B number of bootstrap replications at each fence
#'   boundary value
#' @param n.c number of boundary values to be considered
#' @param n.cores number of cores to be used when parallel
#'   processing the bootstrap (currently only available on
#'   UNIX-type machines, e.g. Mac OS X).
#' @param best.only logical.  A modification to the adaptive
#'   procedure which considers all models at a particular
#'   size that pass the fence hurdle when calculating the
#'   p* values.  The rationale being that if a model is
#'   'correct' then it will be the only model at that size
#'   that passes the fence.  This can help to determine where
#'   the correct peak is to select c*.
#' @param pch plotting 'character', i.e., symbol to use. 
#'   This can either be a single character or an integer 
#'   code for one of a set of graphics symbols.
#' @param initial.stepwise logical.  Performs an initial stepwise 
#'   procedure to look for the range of model sizes where attention
#'   should be focussed. See details for implementation.
#' @param ... optional extra parameters passed through (?? necessary)
#' @seealso \code{\link[mplot]{lmfence}}, \code{\link[mplot]{lmmfence}}
#' @references Jiang et. al. (2009); Tarr, M\:uller and Welsh (2014)
#' @export
#' @family fence
#' @examples
#' n = 100
#' set.seed(11)
#' e = rnorm(n)
#' x0 = 1
#' x1 = rnorm(n)
#' x2 = rnorm(n)
#' x3 = x1^2
#' x4 = x2^2
#' x5 = x1*x2
#' 
#' # Model I
#' yI = x0 + x1 + x2 + e
#' datI = data.frame(yI,x1,x2,x3,x4,x5)
#' af1 = af(yI~., data=datI, n.cores=2)
#' plot(af1)
#' 
#' # Model II
#' yII = x0 + x1 + x2 + x3 + x4 + x5 + e
#' datII = data.frame(yII,x1,x2,x3,x4,x5)
#' af2 = af(yII~., data=datII, n.cores=2)
#' plot(af2)
#' 
#' # Importance of initial.stepwise
#' y3 = x0 + x1 + 10*x2 + x3 + x4 + x5 + e
#' dat3 = data.frame(y3,x1,x2,x3,x4,x5)
#' # if initial.stepwise=FALSE too wide a range of c values is considered
#' # which can result in picking too small a model:
#' af3 = af(y3~.,data=dat3,n.cores=3,initial.stepwise=FALSE)
#' plot(af3)
#' lmfence(y3~.,dat=dat3,cstar=af3$c.star)
#' # if initial.stepwise = TRUE only 'reasonable' values of c are 
#' # considered resulting in the correct model being picked:
#' af4 = af(y3~.,data=dat3,n.cores=3,initial.stepwise=TRUE)
#' plot(af4)
#' lmfence(y3~.,dat=dat3,cstar=af4$c.star)
#' 
#' # Real example
#' bfat.full = af(Bodyfat~.,data=bodyfat,n.cores=3,
#'           n.c=100,initial.stepwise=FALSE)
#' bfat.stepwise = af(Bodyfat~.,data=bodyfat,n.cores=3,
#'           n.c=50,initial.stepwise=TRUE)

af = function(fixed, random, family, data, n.cores,
              method="ML", B=60, n.c=20,
              best.only=FALSE,
              initial.stepwise=TRUE, ...){
  if(!missing(family)){
    if(!require(bestglm)){
      install.packages("bestglm")
    }
    require(bestglm)
  }
  
  cl <- match.call()
  yname = deparse(fixed[[2]])
  null.ff = as.formula(paste(yname,"~1"))
  if(missing(family)){
    mf = lm(fixed, data = data, x=TRUE)# full model
    m0 = lm(null.ff, data = data) # null model
  } else {
    mf = glm(fixed, data = data, family=family) # full model
    m0 = glm(null.ff, data = data, family=family) # null model
  }
  # using this approach to cope when there are indicator
  # variables in the formula, they get spelled out in
  # model.matrix
  X = model.matrix(mf)
  if(colnames(X)[1]=="(Intercept)"){
    X[,1] = data[,yname] # overwrite intercept with y-variable
    no.int = FALSE
  } else {
    X = cbind(data[,yname],X)
  }  
  colnames(X)[1] = yname
  X = data.frame(X)
  
  n = nrow(X)
  k.full = length(mf$coef) 
  
  Qm0 = Qm(m0, method=method) # Qm for the null model
  # add robustness into the redundent variable name
  RED.VAR.DEL = rnorm(n)
  Xstar = data.frame(X,RED.VAR.DEL=RED.VAR.DEL) # full model plus a redundant variable
  full.mod = as.formula(paste(yname,"~."))
  if(missing(family)){
    mfstar = lm(full.mod, data = Xstar) # full model + RED.VAR.DEL
  } else {
    mfstar = glm(full.mod, data = Xstar, family=family) # full model + RED.VAR.DEL
  }
  
  Qmfstar = Qm(mfstar, method=method)
  c.max=c.min=c.range=NA
  if (initial.stepwise) {
    # k.range = krange(mfstar, m0, n, lower=null.ff, upper=full.mod, data=Xstar)
    # took out of a function due to data passing issues
    # backwards and forwards model selection using
    # BIC (conservative) and AIC (less conservative)
    # there is currently an issue with the stepwise procedures
    # and including a factor variable.
    bwds.BIC = step(mfstar, scope = list(lower=null.ff, upper=full.mod),
                    direction="backward", k=log(n), trace=0)
    fwds.BIC = step(m0, scope = list(lower=null.ff, upper=as.formula(mf$model)),
                    direction="forward", k=log(n), trace=0)
    k.min = max(min(length(bwds.BIC$coef),length(fwds.BIC$coef))-1,1)
    bwds.AIC = step(mfstar, scope = list(lower=null.ff, upper=full.mod),
                    direction="backward", k=2, trace=0)
    fwds.AIC = step(m0, scope = list(lower=null.ff, upper=as.formula(mf$model)),
                    direction="forward", k=2, trace=0)
    k.max = min(max(length(bwds.AIC$coef),length(fwds.AIC$coef))+1,k.full)  
    k.range = list(k.min=k.min,k.max=k.max)
    Q.range = qrange(k.range=k.range, data=Xstar, yname=yname, fixed=fixed, method=method)
    c.max = (Q.range$Q.max - Qmfstar)*1.1
    c.min = max(Q.range$Q.min - Qmfstar,0)*0.9
    c.range = seq(c.min,c.max,length.out=n.c)
  } else {
    k.range = list(k.min=1,k.max=k.full)
    c.max = (Qm0-Qmfstar)*1.1
    c.min = 0
    c.range = seq(c.min,c.max,length.out=n.c)
  }
  
  
  #   if(missing(family)){
  #     mu = predict(mfstar)
  #     sighat = summary(mfstar)$sigma
  #   } else {
  #     stop("Need to implement parametric boostrap for glms.")
  #   }
  # Better than above for parametric bootstrap:
  
  fence.mod = list()
  if(missing(n.cores)){
    if(!require(doMC)&!require(foreach)){
      warning("To use parallel programming you need to install \n
              the packages doMC and foreach using \n 
              install.packages(\"doMC\", \"foreach\").")
      n.cores=1
    } else n.cores = max(detectCores()-1,1)
  }
  if(n.cores>1){
    if(.Platform$OS.type!="unix"){
      warning("The parallel programming implementation of the \n
              bootstrap is currently only available on unix-type \n
              systems.  Changing to n.cores=1.")
      n.cores=1
    }
    if(.Platform$GUI=="AQUA"){
      warning("It appears you are running R in a GUI (for example R.app).
              The multicore functionality only works when R is run
              from the command line (using RStudio also works). 
              Setting n.cores=1 (this will be slower).")
      n.cores=1
    }
  }
  if(n.cores>1){
    require(doMC)
    require(foreach)
    registerDoMC(cores=n.cores)
    if(missing(family)){
      p.star = foreach(j = 1:n.c,.combine=rbind) %dopar% {
        fence.mod = list()
        ystar = simulate(object=mfstar,nsim=B)
        for(i in 1:B){
          # ystar = mu + rnorm(n,0,sighat)
          Xstar[,1] = ystar[,i]
          fms = lmfence(fixed=fixed,data=Xstar,
                        cstar=c.range[j],trace=FALSE,
                        best.only=best.only)
          fence.mod = c(fence.mod,fms)
        } # find most frequently arising set of covariates
        # note that the width.cutoff could be an issue for 
        # long variable names and large numbers of variables
        # return to this later to find a more elegant solution
        temp = sort(table(sapply(fence.mod,deparse,
                                 width.cutoff=500)),
                    decreasing=TRUE)
        pstarj = as.numeric(temp[1]/length(fence.mod))
        pstarnamej = names(temp)[1]
        c(pstarj,pstarnamej)
      }
    } else { ### the ONLY DIFFERENCE IS lmfence above vs glmfence below
      p.star = foreach(j = 1:n.c,.combine=rbind) %dopar% {
        fence.mod = list()
        ystar = simulate(object=mfstar,nsim=B)
        for(i in 1:B){
          #ystar = mu + rnorm(n,0,sighat)
          Xstar[,1] = ystar[,i]
          fms = glmfence(fixed=fixed,data=Xstar, family=family,
                         cstar=c.range[j],trace=FALSE,
                         best.only=best.only)
          fence.mod = c(fence.mod,fms)
        } # find most frequently arising set of covariates
        # note that the width.cutoff could be an issue for 
        # long variable names and large numbers of variables
        # return to this later to find a more elegant solution
        temp = sort(table(sapply(fence.mod,deparse,
                                 width.cutoff=500)),
                    decreasing=TRUE)
        pstarj = as.numeric(temp[1]/length(fence.mod))
        pstarnamej = names(temp)[1]
        c(pstarj,pstarnamej)
      }
    }
  } else if(n.cores==1) {
    p.star = matrix(NA,nrow=n.c,ncol=2)
    for(j in 1:n.c){
      fence.mod = list()
      for(i in 1:B){
        ystar = mu + rnorm(n,0,sighat)
        Xstar[,1] = ystar
        if(missing(family)){
          fms = lmfence(fixed=fixed,data=Xstar,
                        cstar=c.range[j],trace=FALSE,
                        best.only=best.only)
        } else {
          fms = glmfence(fixed=fixed,data=Xstar,family=family,
                         cstar=c.range[j],trace=FALSE,
                         best.only=best.only)  
        }
        fence.mod = c(fence.mod,fms)
      } # find most frequently arising set of covariates
      # note that the width.cutoff could be an issue for 
      # long variable names and large numbers of variables
      # return to this later to find a more elegant solution
      temp = sort(table(sapply(fence.mod,deparse,
                               width.cutoff=500)),
                  decreasing=TRUE)
      p.star[j,1] = as.numeric(temp[1]/length(fence.mod))
      p.star[j,2] = names(temp)[1]
    }  
  }
  pstarmods = sort(table(p.star[,2]),decreasing=TRUE)
  n.pstarmods = length(pstarmods)
  p.star = data.frame(pstar = as.numeric(p.star[,1]),
                      model = as.character(p.star[,2]),
                      modelident = match(p.star[,2], names(pstarmods)))
  redundent.vars = grepl("RED.VAR.DEL",as.character(p.star$model))
  c.range = c.range[!redundent.vars]
  p.star = p.star[!redundent.vars,]
  p.star$model = droplevels(p.star$model)
  
  
  # if want runs of (near) maximums
  max.p = max(p.star[,1]) - 2/B
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
  # if we didn't discard the redundent variable earlier on
  # we would need to do something like this:
  #test=vector(length=length(mid))
  #substrRight <- function(x, n){
  #  substr(x, nchar(x)-n+1, nchar(x))
  #}
  #for(i in 1:length(mid)){
  ## check the corresponding model for the presence of RED.VAR.DEL
  #  # if redu is present discount this model from consideration
  #  test[i] = substrRight(p.star[mid[i],2],7)=="RED.VAR.DEL"
  #}
  #c.star = min(c.range[mid[test==FALSE]])
  
  if(missing(family)){
    afmod = lmfence(fixed=fixed,data=X,
                    cstar=c.star,trace=FALSE,
                    best.only=TRUE)  
  } else {
    afmod = glmfence(fixed=fixed,data=X,family=family,
                     cstar=c.star,trace=FALSE,
                     best.only=TRUE)  
  }
  attributes(afmod[[1]]) = NULL
  #afmod = afmod[[1]]
  
  # set up the output object class
  afout = list()
  afout$p.star = p.star
  #colnames(afout$p.star) = c("pstar","model","modelident")
  afout$p.star[,1] = as.numeric(as.character(afout$p.star[,1]))
  afout$c.range = c.range
  afout$c.star = c.star
  afout$model = afmod
  afout$call = cl
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
#' @param x \code{af} object, the result of \code{\link{af}}
#' @export
# S3 method for class 'af'
summary.af = function (x) {
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), 
      "\n\n", sep = "")
  cat("Adaptive fence model (c*=")
  cat(round(x$c.star,1))
  cat("):\n")
  cat(deparse(x$model))
  cat("\n\n")
  cat("Model sizes considered: ")
  cat(x$k.range$k.min)
  cat(" to ")
  cat(x$k.range$k.max)
  cat(" (including intercept).")
  cat("\n\n")
  if(!is.null(x$initial.stepwise)){
    cat("Stepwise procedures:\n")
    cat("Forwards AIC: ")
    cat(deparse(x$initial.stepwise$fwds.AIC))
    cat("\n")
    cat("Backwards AIC: ")
    cat(deparse(x$initial.stepwise$bwds.AIC))
    cat("\n")
    cat("Forwards BIC: ")
    cat(deparse(x$initial.stepwise$fwds.BIC))
    cat("\n")
    cat("Backwards BIC: ")
    cat(deparse(x$initial.stepwise$bwds.BIC))
    cat("\n\n")
  }
  invisible(x)
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
#' @param ... other parameters to be passed through to 
#'   plotting functions.
#' @export
# S3 method for class 'af'
plot.af = function(x,pch,classic=FALSE,html.only=FALSE,width=800,height=400,fontSize=12,...){
  if(!require(googleVis)|classic){
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
    suppressPackageStartupMessages(library(googleVis))
    dat <- matrix(NA, nrow = nrow(x$p.star), ncol = nlevels(x$p.star$model) + 1)
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
    # SEE HERE: http://cran.r-project.org/web/packages/googleVis/vignettes/Using_Roles_via_googleVis.html
    gvis.title = paste("Adaptive fence: c*=",round(x$c.star,1),sep="")
    namefunc <- function(v1) {
      deparse(substitute(v1))
    }
    fplot = gvisScatterChart(data=plot.dat,
                             options=list(title=gvis.title,
                             fontSize=fontSize,
                                          vAxis="{title:'p*',minValue:0,maxValue:1,
                  ticks: [0.0,0.2,0.4,0.6,0.8,1.0]}",
                                          hAxis="{title:'c'}",
                                          axisTitlesPosition="out",
                                          chartArea="{left:50,top:30,width:'60%',height:'80%'}",
                                          width=width, height=height))
    if(html.only){
      return(fplot)
    } else {
      return(plot(fplot))
    }
  }
  ## Experimental using rCharts
  #   require(devtools)
  #   install_github('rCharts', 'ramnathv')
  #   
  #   require(rCharts)
  #   
  #   df = data.frame(ps = x$p.star[,1],
  #                   model = x$p.star[,2],
  #                   cr = x$c.range)
  #   np = nPlot(ps~cr, group='model',data = df,
  #              type = 'scatterChart',ylim=c(0,1))
  #   np$chart(showControls = FALSE)
  #   np$xAxis(axisLabel = 'c')  
  #   np$yAxis(axisLabel = 'p*')  
  #   np$chart(forceY = c(0, 1))
  #   np$chart(forceX = c(floor(min(x$c.range)), 
  #                       floor(max(x$c.range))+1))
  #   np
}


#' Print method for an af object
#' 
#' Prints basic output of the bootstrap results of an 
#' af object.
#' 
#' @param x an \code{af} object, the result of \code{\link{af}}
#' @param ... other parameters to be passed through to 
#'   plotting functions.
#' @export
# S3 method for class 'af'
print.af = function (x, ...) {
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), 
      "\n\n", sep = "")
  cat("Adaptive fence model (c*=")
  cat(round(x$c.star,1))
  cat("):\n")
  cat(deparse(x$model))
  cat("\n\n")
  invisible(x)
}

