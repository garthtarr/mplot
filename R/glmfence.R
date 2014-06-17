#' The fence procedure for generalised linear models
#' 
#' This function implements the fence procedure to
#' find the best generalised linear model.  STILL NEED TO ADD IN THE OPTION
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
#' @param family a description of the error distribution and link 
#'   function to be used in the model. This can be a character 
#'   string naming a family function, a family function or the 
#'   result of a call to a family function. (See 
#'   \code{\link[stats]{family}} for details of family functions.)
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
#' @references Tarr, Müller and Welsh (2014)
#' @export
#' @family fence

glmfence = function(fixed, data, family, cstar,
                    nvmax, method="ML",
                    exhaustive=FALSE,
                    adaptive=TRUE,
                    trace=TRUE,
                    best.only=FALSE,...){
  
  yname = deparse(fixed[[2]])
  null.ff = as.formula(paste(yname,"~1"))
  mf = glm(fixed, data = data, family=family)   # full model
  m0 = glm(null.ff, data = data, family=family) # null model
  Qmf = Qm(mf, method=method) # Qm for the full model
  Qm0 = Qm(m0, method=method) # Qm for the null model
  kf = length(mf$coef) # number of parameters in the full model
  
  X = model.matrix(mf)
  if(colnames(X)[1]=="(Intercept)"){
    X = X[,-1] # remove the intercept column
    no.int = FALSE
  }
  Xnames = colnames(X)
  Xy = cbind(X,data[,yname])  
  colnames(Xy) = c(Xnames,yname)
  Xy = data.frame(Xy)
  n = nrow(Xy)
  
  if(kf > 18 & family != "gaussian"){
    warning("This could take a while...")
  }
  
  flag=FALSE
  
  if(missing(nvmax)) nvmax=kf
  ret = list()
  
  # Null model
  if(trace) cat(paste("Null model "))
  UB = Qmf + cstar*sigMM(k.mod=1, method, k.full=kf,adaptive=adaptive)
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
  
  
  # look around for the best model at each model size
  leaps.candidates = bestglm(Xy=Xy, family=m0$family,
                             IC = "BIC",
                             TopModels = 1,
                             nvmax = nvmax)$Subsets[,1:kf]+0
  for(i in 2:nvmax){
    if(trace) cat(paste("Model size:",i,""))
    UB = Qmf + cstar*sigMM(k.mod=i, method, k.full=kf,adaptive=adaptive)
    mnames = colnames(leaps.candidates)[which(leaps.candidates[i,]==1)]
    ff = paste(yname," ~ ",paste(mnames[-1],collapse="+"),sep="")
    ff = as.formula(ff)
    em = glm(formula=ff, data=Xy, family=family)
    hatQm = Qm(em,method=method)
    if(hatQm<=UB){
      if(trace){
        cat("\n")
        cat("Candidate model found via leaps. \n")
        cat("Exploring other options at this model size. ")
      }
      new.leaps.candidates = bestglm(Xy=Xy, family=m0$family,
                                     IC = "BIC",
                                     TopModels = max(5,ceiling(log(i))*2),
                                     nvmax = i-1)$Subsets[,1:kf]+0
      leaps.fixed.models = matrix(new.leaps.candidates[apply(new.leaps.candidates,1,sum)==i,],ncol=kf)
      for(j in 1:dim(leaps.fixed.models)[1]){
        mnames = colnames(new.leaps.candidates)[which(leaps.fixed.models[j,]==1)]
        ff = paste(yname," ~ ",paste(mnames[-1],collapse="+"),sep="")
        ff = as.formula(ff)
        em = glm(formula=ff, data=Xy, family=family)
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
            em = glm(formula=ret[[i]], data=Xy, family=family)
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
  if(flag==FALSE) cat(" (No candidate models found) \n")
}

