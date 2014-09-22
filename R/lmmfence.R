#' The fence procedure for linear mixed models
#' 
#' Relies on a varient of the wild bootstrap in that
#' it only considers the fixed effects.
#' 

lmmfence = function(mf, cstar,
                    nvmax, method="ML",
                    adaptive=TRUE,
                    trace=TRUE,
                    force.in=NULL,...){
  if(class(mf)=="lmerMod"){ # lme4 package
    X = model.matrix(mf)
    data = data.frame(model.frame(mf)) # or slot(mf, "frame")
    if(isGLMM(mf)){
      family = family(mf) # 
      # pass to glm ?? 
      # i.e. wild bootstrap appropriate for glmm's too?
      # though should really call glmer for the full model
      # so it may be the case that class(mf) won't be lmerMod
      # if the family argument was passed to it as lmer() 
      # calls glmer() if there is a family argument present
    }
    yname = deparse(formula(mf)[[2]])
    X = data.frame(data[,yname],X)
    colnames(X)[1] = yname
    fixed.formula = paste(yname,"~",
                          paste(names(fixef(mf))[-1],collapse="+"))
    lmf = lm(fixed.formula,data = X)
    lmfence(lmf,cstar=cstar,
            nvmax=nvmax, method=method,
            adaptive=adaptive,
            trace=trace,
            force.in=force.in,...)
  } else if(class(mf)=="lme"){ # nlme package
    data = mf$data
    X = model.matrix(mf,data = data)
    # nlme package assumes gaussian errors 
    yname = deparse(formula(mf)[[2]])
    X = data.frame(data[,yname],X)
    colnames(X)[1] = yname
    fixed.formula = paste(yname,"~",
                          paste(names(coef(mf))[-1],collapse="+"))
    lmf = lm(fixed.formula, data = X)
    lmfence(lmf,cstar=cstar,
            nvmax=nvmax, method=method,
            adaptive=adaptive,
            trace=trace,
            force.in=force.in,...)
  }
}