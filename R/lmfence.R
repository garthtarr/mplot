#' The fence procedure for linear models
#' 
#' This function implements the fence procedure to
#' find the best linear model.  STILL NEED TO ADD IN THE OPTION
#' TO FORCE VARIABLES INTO THE MODEL (THIS SHOULD BE EASY TO 
#' IMPLEMENT).
#' 
#' @param fixed an object of class \code{\link[stats]{formula}}
#'   specifying the full model that can be fitted.
#' @param data a data frame containing the variables in the full
#'   model.  Currently this is required.  Future development will
#'   look at the \code{lm} function to replicate functionality,
#'   whereby if \code{data} is missing or variables are not found 
#'   in \code{data}, the variables are taken from 
#'   \code{environment(formula)}, typically the
#'   environment from which \code{lmfence} is called.
#' @param cstar the boundary of the fence, typically found
#'   through bootstrapping.
#' @param nvmax the maximum number of variables that will be
#'   be considered in the model (the default is the number of 
#'   explanatory variables in \code{fixed}).
#' @param method the model selection method to be used. Currently
#'   only \code{method = "ML"} is supported (perhaps in the future
#'   \code{method = "MVC"} will be implemented).
#' @param exhaustive logical. If \code{TRUE} the search for a best
#'   model contines through the entire model space and returns 
#'   all models that passed the hurdle at all model sizes
#'   (rather than stopping at the smallest possible model that 
#'   passes the fence hurdle).  It was something Garth used in 
#'   development priorto the incorporating leaps functionality and 
#'   should probably be retired.
#' @param use.leaps logical. If \code{TRUE} the \code{regsubsets} 
#'   function from the \code{leaps} package is used to find the best
#'   candidate model at each dimension.  Then the fence criteria is 
#'   sequentially applied to each of these "best" models.  Once a
#'   candidate model is found, other models at that dimension are 
#'   checked.  Garth can't think of a reason why this wouldn't always
#'   be the preferred option - so perhaps the full search code 
#'   should be retired and use.leaps removed as an option (forced 
#'   \code{TRUE}).  Note that if the number of parameters is large
#'   it is required that \code{use.leaps = TRUE}.
#' @param adaptive logical. If \code{TRUE} the boundary of the fence is 
#'   given by cstar.  Otherwise, it the original (non-adaptive) fence
#'   is performed where the boundary is cstar*hat(sigma)_{M,tildeM}.
#' @param trace logical. If \code{TRUE} the function prints out its 
#'   progress as it iterates up through the dimensions.
#' @param best.only logical.  If \code{TRUE} the function returns 
#'   only one model - the "best" one at the smallest model size for
#'   which there is (at least) one candidate that passes the fence.
#'   Otherwise it returns all candidate models which pass the fence
#'   at that model size.
#' @param ... optional extra parameters passed through (?? necessary)
#' @seealso \code{\link{adaptfence}}, \code{\link{lmmfence}}
#' @references Jiang et. al. (2008); Tarr, Müller and Welsh (2014)
#' @export
#' @family fence
#' @examples
#' n = 40 # sample size
#' beta = c(1,2,3,0,0)
#' K=length(beta)
#' set.seed(198)
#' X = cbind(1,matrix(rnorm(n*(K-1)),ncol=K-1))
#' e = rnorm(n)
#' y = X%*%beta + e
#' dat = data.frame(y,X[,-1])
#' # Non-adaptive approach (not recommended)
#' lmfence(y~.,data=dat,cstar=log(n),adaptive=FALSE)

lmfence = function(fixed, data, cstar,
                   nvmax, method="ML",
                   exhaustive=FALSE,
                   use.leaps=TRUE,
                   adaptive=TRUE,
                   trace=TRUE,
                   best.only=FALSE,...){
  
  
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
  n <- nrow(X)
  k.full <- length(mf$coef) 
  
  if(k.full > 18 & use.leaps == FALSE){
    warning("Too many variables: changed use.leaps=TRUE")
    use.leaps=TRUE
  }
  
  null.ff = as.formula(paste(yname,"~1"))
  m0 = lm(null.ff, data = data) # null model
  Qmf = Qm(mf, method=method) # Qm for the full model
  Qm0 = Qm(m0, method=method) # Qm for the null model
  
  flag=FALSE
  
  if(missing(nvmax)) nvmax=k.full
  ret = list()
  
  # Null model
  if(trace) cat(paste("Null model "))
  UB = Qmf + cstar*sigMM(k.mod=1, method, k.full=k.full,adaptive=adaptive)
  if(Qm0<=UB){
    if(trace){
      cat("\n")
      cat(paste("hatQm:", round(Qm0,2),"; Upper bound:", round(UB,2)),"\n")
      cat(paste("hatQm <= UB:",Qm0<=UB,"\n"))
      cat(deparse(formula(m0)))
      cat("\n")
    }
    flag = TRUE
  }
  if(flag==TRUE & exhaustive==FALSE) return(formula(null.ff))
  if(flag==FALSE & trace) cat("(Not a candidate model) \n")
  
  if(use.leaps==TRUE){
    if(trace) cat(paste("Initial search for best model at each model size up to nvmax=",nvmax,"\n",sep=""))
    # finds the best candidate for each model size
    leaps.candidates = summary(regsubsets(fixed, 
                                          data = data, 
                                          nbest = 1,
                                          nvmax = nvmax))$which+0
    
    for(i in 2:nvmax){
      if(trace) cat(paste("Model size:",i,""))
      UB = Qmf + cstar*sigMM(k.mod=i, method, k.full=k.full,adaptive=adaptive)
      mnames = colnames(leaps.candidates)[which(leaps.candidates[i-1,]==1)]
      ff = paste(yname," ~ ",paste(mnames[-1],collapse="+"),sep="")
      ff = as.formula(ff)
      em = lm(formula=ff, data=X)
      hatQm = Qm(em,method=method)
      if(hatQm<=UB){
        if(trace){
          cat("\n")
          cat("Candidate model found via leaps. \n")
          cat("Exploring other options at this model size. ")
        }
        new.leaps.candidates = summary(regsubsets(fixed,
                                                  data=data,
                                                  nbest = ceiling(log(i))*2,
                                                  nvmax = i-1))$which+0
        leaps.fixed.models = matrix(new.leaps.candidates[apply(new.leaps.candidates,1,sum)==i,],ncol=k.full)
        for(j in 1:dim(leaps.fixed.models)[1]){
          mnames = colnames(new.leaps.candidates)[which(leaps.fixed.models[j,]==1)]
          ff = paste(yname," ~ ",paste(mnames[-1],collapse="+"),sep="")
          ff = as.formula(ff)
          em = lm(formula=ff, data=X)
          hatQm = Qm(em,method=method)
          if(hatQm<=UB){
            if(trace){
              cat("\n")
              cat(paste("hatQm:", round(hatQm,2),"; Upper bound:", round(UB,2)),"\n")
              cat(paste("hatQm <= UB:",hatQm<=UB,"\n"))
              cat(deparse(formula(em)))
              cat("\n")
            }
            ret = c(ret,formula(em))
          }
          
        }
        if(adaptive==TRUE) {
          if(best.only==TRUE){
            met = NA
            for(i in 1:length(ret)){
              em = lm(formula=ret[[i]], data=X)
              met[i] = Qm(em,method=method)
            }
            return(ret[[which(met==min(met))]])
          } else if(best.only==FALSE){
            return(ret)
          }
        } else if(adaptive==FALSE) return(ret)
      }
      if(trace) cat("(No candidate models found) \n")
    }
  } else if(use.leaps==FALSE){ 
    # Remaining possibilities manually
    ret = list()
    for(i in 2:nvmax){
      var.name.mat = combn(names(mf$coef)[-1],i-1)
      if(flag==FALSE & i>2) cat(" (No candidate models found) \n")
      cat(paste("Model size:",i))
      UB = Qmf + cstar*sigMM(k.mod=i, method, k.full=k.full,adaptive=adaptive)
      for(j in 1:dim(var.name.mat)[2]){
        #mframe = data.frame(X[,which(ms[j,]==1)])
        #names(mframe) = names(mf$coef)[which(ms[j,]==1)] unnecessary now that null model is done separately
        #ff = paste(names(mf$coef)[1]," ~ ",paste(names(mframe)[-1],collapse="+"),sep="")
        ff = paste(yname," ~ ",paste(var.name.mat[,j],collapse="+"),sep="")
        ff = as.formula(ff)
        em = lm(formula=ff, data=X)
        hatQm = Qm(em,method=method)
        if(hatQm<=UB){
          cat("\n")
          cat(paste("hatQm:", round(hatQm,2),"; Upper bound:", round(UB,2)),"\n")
          cat(paste("hatQm <= UB:",hatQm<=UB,"\n"))
          cat(deparse(formula(em)))
          cat("\n")
          flag = TRUE
          ret = c(ret,formula(em))
        } 
      }
      if(adaptive==TRUE) {
        if(best.only==TRUE){
          met = NA
          for(i in 1:length(ret)){
            em = lm(formula=ret[[i]], data=X)
            met[i] = Qm(em,method=method)
          }
          return(ret[[which(met==min(met))]])
        } else if(best.only==FALSE){
          return(ret)
        }
      }
      if(flag==TRUE & exhaustive==FALSE) return(ret)
    }
  }
  if(flag==FALSE) cat(" (No candidate models found) \n")
}
