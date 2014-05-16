#' Fence based model selection
#' 
#' A wrapper function that combines \code{lmfence} (or, in the 
#' future \code{lmmfence}) and \code{af} that performs the adaptive
#' fence procedure in an automated way, then extracts the optimal model
#' and presents diagnostics.

#' Implements the fence of Jiang (2008)
#' Assumes number of variables << number of observations
#' and that the full model is estimable
#' also currently assumes the random effects are known
#' i.e. only does model selection for fixed effects
#' 
#' @param fixed formula specifying the full fixed model
#' @param random (optional) formula specifying the full random model
#'   using the syntax of lme() #MAY CHANGE TO lme4()#
#' @param data data frame containing all (fixed and random) variables
#' @param method the model selection method to be used. Currently
#'   only \code{method = "ML"} is supported (perhaps in the future
#'   \code{method = "MVC"} will be implemented).
#' @param adaptive logical. If \code{TRUE} the boundary of the fence is 
#'   given by cstar.  Otherwise, it the original (non-adaptive) fence
#'   is performed where the boundary is cstar*hat(sigma)_{M,tildeM}.
#' @param B number of bootstrap replications for the adaptive fence
#' 
# exhaustive:
# use.leaps: 
# intercept: If TRUE, an intercept is included in the model, 
#            otherwise no intercept is included. Default is TRUE.
# normalize: If TRUE, each variable is standardized to have unit 
#            variance, otherwise it is left alone. Default is TRUE.
#            If you have indicator variables, you may like to perform
#            your own standardisation and set normalize to FALSE
fence = function(fixed, random, data,
                 method = c("ML", "MVC"),
                 adaptive=TRUE,
                 B = 100, R = 100,
                 nvmax, trace = FALSE,
                 exhaustive = FALSE,
                 use.leaps = FALSE,
                 normalize = TRUE,
                 intercept = TRUE){
  call <- match.call()
  method <- match.arg(method)
  METHOD <- switch(method, 
                   ML = "ML", 
                   MVC = "MVC")
  
  one <- rep(1, n)
  vn <- dimnames(x)[[2]]
  if (intercept) {
    meanx <- drop(one %*% x)/n
    x <- scale(x, meanx, FALSE)
    mu <- mean(y)
    y <- drop(y - mu)
  }
  else {
    meanx <- rep(0, m)
    mu <- 0
    y <- drop(y)
  }
  if (normalize) {
    normx <- sqrt(drop(one %*% (x^2)))
    nosignal <- normx/sqrt(n) < eps
    if (any(nosignal)) {
      ignores <- im[nosignal]
      inactive <- im[-ignores]
      normx[nosignal] <- eps * sqrt(n)
      if (trace) 
        cat("LARS Step 0 :\t", sum(nosignal), "Variables with Variance < eps; dropped for good\n")
    }
    else ignores <- NULL
    names(normx) <- NULL
    x <- scale(x, FALSE, normx)
  } else {
    normx <- rep(1, m)
    ignores <- NULL
  }
  
  
  if(method=="MVC"){
    warning("Mean and variance/covariance (MVC) model selection \n  
            not yet implemented, defaulting to Maximum Likelihood")
    method = "ML"
  }
  
  #if(missing(cn)){ 
  #  warning("cn factor is missing.  Current (bad) default: cn=sqrt(n)")
  #}
  
  if(!missing(cn)){
    if(cn!="adaptive") {
      warning("You appear to be using a fixed cn factor. 
              You’ve got to ask yourself one question: 
              'Do I feel lucky?' Well, do ya, punk?")
    } else if (cn=="adaptive") {
      cn = boot.cn(fixed=fixed,random=random,
                   method=method,B=B,R=R,
                   use.leaps=TRUE)
    }
  }
  
  if(missing(cn)) cn=sqrt(n)
  
  if(missing(random)) lmfence(fixed=fixed,data=data,
                              method=method,cn=cn,
                              exhaustive=exhaustive,
                              use.leaps=use.leaps,
                              nvmax=nvmax)
  else lmmfence(fixed=fixed,random=random,data=data,
                method=method,cn=cn,B=B,
                exhaustive=exhaustive)
  
}  