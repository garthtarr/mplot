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
#' @param pch plotting ‘character’, i.e., symbol to use. 
#'   This can either be a single character or an integer 
#'   code for one of a set of graphics symbols.
#' @param initial.stepwise logical.  Performs an initial stepwise 
#'   procedure to look for the range of model sizes where attention
#'   should be focussed. See details for implementation.
#' @param ... optional extra parameters passed through (?? necessary)
#' @seealso \code{\link{lmfence}}, \code{\link{lmmfence}}
#' @references Jiang et. al. (2009); Tarr, Müller and Welsh (2014)
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
#' bfat = af(Bodyfat~.,data=bodyfat,n.cores=3,
#'           n.c=100,initial.stepwise=FALSE)

af = function(fixed, random, data,
              method="ML", B=60, n.c=20,
              n.cores=1,
              best.only=FALSE,
              initial.stepwise=TRUE, ...){
  
  yname = deparse(fixed[[2]])
  mf = lm(fixed, data = data) # full model
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
  null.ff = as.formula(paste(yname,"~1"))
  m0 = lm(null.ff, data = data) # null model
  Qm0 = Qm(m0, method=method) # Qm for the null model
  red.var = rnorm(n)
  Xstar = data.frame(X,red.var=red.var) # full model plus a redundant variable
  full.mod = as.formula(paste(yname,"~."))
  mfstar = lm(full.mod, data = Xstar) # full model + red.var
  Qmfstar = Qm(mfstar, method=method)
  c.max=c.min=c.range=NA
  if (initial.stepwise) {
    # k.range = krange(mfstar, m0, n, lower=null.ff, upper=full.mod, data=Xstar)
    # took out of a function due to data passing issues
    # backwards model selection using BIC
    bwards = step(mfstar, scope = list(lower=null.ff, upper=full.mod),
                  direction="backward", k=log(n), trace=0)
    k.min = max(length(bwards$coef)-1,1)
    # forwards model selection using AIC
    fwards = step(m0, scope = list(lower=null.ff, upper=full.mod),
                  direction="backward", k=2, trace=0)
    k.max = min(length(bwards$coef)+1,k.full)  
    k.range = list(k.min=k.min,k.max=k.max)
    Q.range = qrange(k.range=k.range, data=Xstar, yname=yname, fixed=fixed, method=method)
    c.max = (Q.range$Q.max - Qmfstar)*1.1
    c.min = max(Q.range$Q.min - Qmfstar,0)*0.9
    c.range = seq(c.min,c.max,length.out=n.c)
  } else {
    c.max = (Qm0-Qmfstar)*1.1
    c.min = 0
    c.range = seq(c.min,c.max,length.out=n.c)
  }
  
  
  mu = predict(mfstar)
  sighat = summary(mfstar)$sigma
  fence.mod = list()
  if(n.cores>1){
    if(!require(doMC)&!require(foreach)){
      warning("To use parallel programming you need to install \n
              the packages doMC and foreach using \n 
              install.packages(\"doMC\", \"foreach\").")
      n.cores=1
    }
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
              Changing to n.cores=1 (this will be slower).")
      n.cores=1
    }
    registerDoMC(cores=n.cores)
    p.star = foreach(j = 1:n.c,.combine=rbind) %dopar% {
      fence.mod = list()
      for(i in 1:B){
        ystar = mu + rnorm(n,0,sighat)
        Xstar[,1] = ystar
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
  } 
  if(n.cores==1) {
    p.star = matrix(NA,nrow=n.c,ncol=2)
    for(j in 1:n.c){
      fence.mod = list()
      for(i in 1:B){
        ystar = mu + rnorm(n,0,sighat)
        Xstar[,1] = ystar
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
      p.star[j,1] = as.numeric(temp[1]/length(fence.mod))
      p.star[j,2] = names(temp)[1]
    }  
  }
  pstarmods = sort(table(p.star[,2]),decreasing=TRUE)
  n.pstarmods = length(pstarmods)
  p.star = cbind(p.star,match(p.star[,2], names(pstarmods)))
  
  # if want runs of maximums
  tf = p.star[,1] == max(p.star[,1])
  # if want runs of very high vaues of p*
  # e.g. p*>1-1/sqrt(B)
  # tf = p.star[,1] > 1-1/sqrt(B)
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
  test=vector(length=length(mid))
  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }
  for(i in 1:length(mid)){
    # check the corresponding model for the presence of red.var
    # if redu is present discount this model from consideration
    test[i] = substrRight(p.star[mid[i],2],7)=="red.var"
  }
  c.star = min(c.range[mid[test==FALSE]])
  
  # set up the output object class
  afout = list()
  class(afout) = "af"
  afout$p.star = data.frame(p.star)
  colnames(afout$p.star) = c("pstar","model","modelident")
  afout$p.star[,1] = as.numeric(as.character(afout$p.star[,1]))
  afout$c.range = c.range
  afout$c.star = c.star
  return(afout)
}


#' Plot diagnostics for an af object
#' 
#' Summary plot of the bootstrap results of an 
#' af object.
#' 
#' @param x \code{af} object, the result of \code{\link{af}}
#' @param ... other parameters to be passed through to 
#'   plotting functions.
# S3 method for class 'af'
#setMethod("plot", signature(x="af"),
plot.af = function(x,pch,...){
  if(missing(pch)) pch=19
  plot(x$p.star[,1]~x$c.range,
       ylim=c(0,1), pch=pch,
       col=x$p.star[,3],
       ylab = "p*", xlab = "c")
  legend("bottomleft",legend=unique(x$p.star[,2]),
         pch=pch,col=unique(x$p.star[,3]),bty="n")
  axis(side=3, at=x$c.star, 
       labels=paste("c*=", round(x$c.star,1),sep=""))
}
#' Print method for an af object
#' 
#' Prints basic output of the bootstrap results of an 
#' af object.
#' 
#' @param x \code{af} object, the result of \code{\link{af}}
#' @param ... other parameters to be passed through to 
#'   plotting functions.
# S3 method for class 'af'
# print.af = function(x,...)

#' Summary method for an af object
#' 
#' Provides comprehensive  output of the bootstrap results of an 
#' af object.
#' 
#' @param x \code{af} object, the result of \code{\link{af}}
#' @param ... other parameters to be passed through to 
#'   plotting functions.
# S3 method for class 'af'
# summary.af = function(x,...)

