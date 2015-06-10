#' Graphical model stability and model selection procedures
#' 
#' @name mplot-package
#' @docType package
#' @title Graphical model stability and model selection procedures
#' @keywords package
NULL


#' Body fat data set
#' 
#' A data frame with 128 observations on 15 variables.
#' 
#' @name bodyfat
#' @format A data frame with 128 observations on 15 variables.
#' \describe{
#' \item{Id}{Identifier}
#' \item{Bodyfat}{Bodyfat percentage}
#' \item{Age}{Age (years)}
#' \item{Weight}{Weight (kg)}
#' \item{Height}{Height (inches)}
#' \item{Neck}{Neck circumference (cm)}
#' \item{Chest}{Chest circumference (cm)}
#' \item{Abdo}{Abdomen circumference (cm) "at the umbilicus 
#'            and level with the iliac crest"}
#' \item{Hip}{Hip circumference (cm)}
#' \item{Thigh}{Thigh circumference (cm)}
#' \item{Knee}{Knee circumference (cm)}
#' \item{Ankle}{Ankle circumference (cm)}
#' \item{Bic}{Extended biceps circumference (cm)}
#' \item{Fore}{Forearm circumference (cm)}
#' \item{Wrist}{Wrist circumference (cm) "distal to the
#'             styloid processes"}
#' }
#' @details A subset of the 252 observations available in the \code{mfp} package.
#'   The selected observations avoid known high leverage points and
#'   outliers.  The unused points from the data set could be used to validate 
#'   selected models.
#' @docType data
#' @keywords datasets
#' @usage data(bodyfat)
#' @references Johnson W (1996, Vol 4). Fitting percentage of 
#'   body fat to simple body measurements. Journal of Statistics 
#'   Education. Bodyfat data retrieved from 
#'   http://www.amstat.org/publications/jse/v4n1/datasets.johnson.html
#'   An expanded version is included in the \code{mfp} R package.
#' @examples
#' data(bodyfat)
#' full.mod = lm(Bodyfat~.,data=subset(bodyfat,select=-Id))
NULL


#' Blood and other measurements in diabetics
#' 
#' The diabetes data frame has 442 rows and 11 columns. 
#' These are the data used in Efron et al. (2004).
#' 
#' @name diabetes
#' @format A data frame with 442 observations on 11 variables.
#' \describe{
#' \item{age}{Age}
#' \item{sex}{Gender}
#' \item{bmi}{Body mass index}
#' \item{map}{Mean arterial pressure (average blood pressure)}
#' \item{tc}{Total cholesterol (mg/dL)? Desirable range: below 200 mg/dL}
#' \item{ldl}{Low-density lipoprotein ("bad" cholesterol)? Desirable range: below 130 mg/dL }
#' \item{hdl}{High-density lipoprotein ("good" cholesterol)? Desirable range: above 40 mg/dL}
#' \item{tch}{Blood serum measurement}
#' \item{ltg}{Blood serum measurement}
#' \item{glu}{Blood serum measurement (glucose?)}
#' \item{y}{A quantitative measure of disease progression one year after baseline}
#' }
#' @details Data sourced from http://web.stanford.edu/~hastie/Papers/LARS
#' @docType data
#' @keywords datasets
#' @usage data(diabetes)
#' @references Efron, B., Hastie, T., Johnstone, I., Tibshirani, R., (2004).
#'   Least angle regression. The Annals of Statistics 32(2) 407-499.
#'   DOI: 10.1214/009053604000000067
#' @examples
#' data(diabetes)
#' full.mod = lm(y~.,data=diabetes)
NULL




#' Artificial example
#' 
#' An artificial data set which causes stepwise regression
#' procedures to select a non-parsimonious model.
#' The true model is a simple linear regression of
#' y against x8.
#' 
#' @name artificialeg
#' @format A data frame with 50 observations on 10 variables.
#' @details Inspired by the pathoeg data set in the MPV pacakge.
#' @docType data
#' @keywords datasets
#' @usage data(artificialeg)
#' @examples
#' data(artificialeg)
#' full.mod = lm(y~.,data=artificialeg)
#' step(full.mod)
#' \dontrun{
#' # generating model
#' n=50
#' set.seed(8) # a seed of 2 also works
#' x1 = rnorm(n,0.22,2)
#' x7 = 0.5*x1 + rnorm(n,0,sd=2)
#' x6 = -0.75*x1 + rnorm(n,0,3)
#' x3 = -0.5-0.5*x6 + rnorm(n,0,2)
#' x9 = rnorm(n,0.6,3.5)
#' x4 = 0.5*x9 + rnorm(n,0,sd=3)
#' x2 = -0.5 + 0.5*x9 + rnorm(n,0,sd=2)
#' x5 = -0.5*x2+0.5*x3+0.5*x6-0.5*x9+rnorm(n,0,1.5)
#' x8 = x1 + x2 -2*x3 - 0.3*x4 + x5 - 1.6*x6 - 1*x7 + x9 +rnorm(n,0,0.5)
#' y = 0.6*x8 + rnorm(n,0,2)
#' artificialeg = round(data.frame(x1,x2,x3,x4,x5,x6,x7,x8,x9,y),1)
#' }
NULL





#' Extract model elements
#' 
#' This function extracts things like the formula,
#' data matrix, etc. from a lm or glm object
#' 
#' @param model a fitted 'full' model, the result of a call
#'   to lm or glm (and in the future lme or lmer).
#' @param screen logical, whether or not to perform an initial
#'   screen for outliers.  Highly experimental, use at own risk.
#'   Default = FALSE.
#' @param redundant logical, whether or not to add a redundant
#'   variable.  Default = TRUE.
#' @noRd
mextract = function(model,screen=FALSE,redundant=TRUE){
  # what's the name of the dependent variable?
  yname = deparse(formula(model)[[2]])
  # Set up the data frames for use
  data = model.frame(model)
  X = model.matrix(model)
  n=nrow(X)
  # full model plus redundant variable
  exp.vars=names(model$coefficients)[names(model$coefficients)!="(Intercept)"]
  
  if(redundant){
    REDUNDANT.VARIABLE = rnorm(n)
    X = cbind(X,REDUNDANT.VARIABLE)
    data = cbind(data,REDUNDANT.VARIABLE)
    exp.vars = c(exp.vars,"REDUNDANT.VARIABLE")
  }
  if(colnames(X)[1]=="(Intercept)"){
    # overwrite intercept with y-variable
    X[,1] = model.frame(model)[,yname] 
  } else {
    X = cbind(model.frame(model)[,yname],X)
  }
  colnames(X)[1] = yname
  X = data.frame(X)
  fixed = as.formula(c(paste(yname,"~"),
                       paste(colnames(X)[-1],collapse="+")))
  Xy = X[c(2:ncol(X),1)]
  
  k = length(exp.vars) + 1 # +1 for intercept 
  if(screen){
    if (!requireNamespace("mvoutlier", quietly = TRUE)) {
      stop("mvoutlier package needed when screen=TRUE. Please install it.",
           call. = FALSE)
    }
    x.mad = apply(Xy, 2, mad)
    Xy.sub = Xy[,which(x.mad != 0)]
    Xy = Xy[mvoutlier::pcout(Xy.sub)$wfinal01==1,]
    n=dim(Xy)[1]
    if(k>=n){
      warning("Screening deleted too many observations.")
      return()
    }
  }
  wts = model$weights
  if(is.element("glm",class(model))){
    wts = model$prior.weights
    Xy[,yname] = model$y
  } 
  if(is.null(wts)){
    wts = rep(1,n)
  }
  
  return(list(yname=yname,
              fixed=fixed,
              wts = wts,
              X=Xy,
              k = k,
              n=n,
              exp.vars=exp.vars,
              data = data,
              family = family(model)))
  
  # MIXED MODELS NOT IMPLEMENTED IN FIRST RELEASE   
  #   if(class(model)=="lmerMod"){ # lme4 package
  #     X = model.matrix(model)
  #     data = data.frame(model.frame(model)) # or slot(model, "frame")
  #     if(isGLMM(model)){
  #       family = family(model) # 
  #       # pass to glm ?? 
  #       # i.e. wild bootstrap appropriate for glmm's too?
  #       # though should really call glmer for the full model
  #       # so it may be the case that class(model) won't be lmerMod
  #       # if the family argument was passed to it as lmer() 
  #       # calls glmer() if there is a family argument present
  #     }
  #     yname = deparse(formula(model)[[2]])
  #     X = data.frame(data[,yname],X)
  #     colnames(X)[1] = yname
  #     fixed.formula = paste(yname,"~",
  #                           paste(names(fixef(model))[-1],collapse="+"))
  #     model = lm(fixed.formula,data = X)
  #   } else if(class(model)=="lme"){ # nlme package
  #     data = model$data
  #     X = model.matrix(model,data = data)
  #     # nlme package assumes gaussian errors 
  #     yname = deparse(formula(model)[[2]])
  #     X = data.frame(data[,yname],X)
  #     colnames(X)[1] = yname
  #     fixed.formula = paste(yname,"~",
  #                           paste(names(coef(model))[-1],collapse="+"))
  #     model = lm(fixed.formula, data = X)
  #   }
}

#' Safe deparse
#' 
#' Supports long formula construction
#' 
#' @param expr expression to be safely deparsed
#' 
#' @noRd
safeDeparse <- function(expr){
  ret <- paste(deparse(expr), collapse="")
  #rm whitespace
  gsub("[[:space:]][[:space:]]+", " ", ret)
}

#' Print text for fence methods
#' 
#' This function provides the text for the case when trace=TRUE
#' when using lmfence and glmfence functions.
#' 
#' @param score realised value
#' @param UB upper bound
#' @param obj fitted model object
#' @keywords internal
txt.fn = function(score,UB,obj){
  cat("\n")
  cat(paste("hatQm:", round(score,2),"; Upper bound:", round(UB,2)),"\n")
  cat(paste("hatQm <= UB:",score<=UB,"\n"))
  cat(deparse(formula(obj)))
  cat("\n")
}

#' Process results within af function
#' 
#' This function is used by the af function to process
#' the results when iterating over different boundary values
#' 
#' @param fence.mod set of fence models
#' @param fence.rank set of fence model ranks
#' @noRd
process.fn = function(fence.mod,fence.rank){
  del2 = function(x) x[-c(1:2)]
  splitAt <- function(x, pos) unname(split(x, cumsum(seq_along(x) %in% pos)))
  
  # best only (true fence)
  fence.mod.bo = fence.mod[fence.rank==1]
  #  temp.bo = sort(table(sapply(fence.mod.bo,deparse,
  #                              width.cutoff=500)), decreasing=TRUE)
  temp.bo = sort(table(unlist(lapply(lapply(fence.mod.bo,as.character),del2))),
                 decreasing=TRUE)
  pstarj.bo = as.numeric(temp.bo[1]/length(fence.mod.bo))
  pstarnamej.bo = names(temp.bo)[1]
  
  # all that pass the fence
  #temp.all = sort(table(sapply(fence.mod,deparse,
  #                             width.cutoff=500)),decreasing=TRUE)
  temp.all = sort(table(unlist(lapply(lapply(fence.mod,as.character),del2))),
                  decreasing=TRUE)
  #old version
  #pstarj.all = as.numeric(temp.all[1]/length(fence.mod))
  #pstarnamej.all = names(temp.all)[1]
  # new version
  unlist.fence.rank = unlist(fence.rank)
  fence.rank.split = splitAt(unlist.fence.rank,which(unlist.fence.rank==1))
  custom.p = 1/unlist(lapply(fence.rank.split,length))
  custom.names = unlist(lapply(lapply(fence.mod.bo,as.character),del2))
  agg = aggregate(custom.p,by=list(custom.names),sum)
  agg = agg[order(agg$x,decreasing = TRUE),]
  pstarj.all = agg[1,2]/sum(unlist.fence.rank==1)
  pstarnamej.all = agg[1,1]
  
  return(c(pstarj.bo,pstarnamej.bo,pstarj.all,pstarnamej.all))
}


