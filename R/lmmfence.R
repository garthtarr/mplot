lmmfence = function(fixed,random,method,data,cn,B,exhaustive){
  
  #trim.ws <- function(text) gsub("^[\ \t]", "", gsub("[\ \t]*$", "", text))
  #RHS.s = trim.ws(strsplit(RHS,fixed=TRUE,split="+")[[1]])
  
  X = model.frame(fixed,data=data)
  X = X[,!(names(X) %in% all.vars(random))] # fixed design matrix
  # y = X[,1] # dependent variable obtained from fixed formula
  k.full = dim(X)[2] # p+1 (includes intercept)
  n = dim(X)[1]
  RHS = deparse(fixed[[3]])
  "%w/o%" = function(x, y) x[!x %in% y]
  if(substr(RHS,1,1)=="."){
    LHS = paste(fixed[[2]],fixed[[1]])
    RHS = paste(names(X[,-1]) %w/o% all.vars(fixed[[3]]),collapse="+")
    full.ff = as.formula(paste(LHS,RHS))
    # try full.ff = formula(X) because X is fixed design matrix
  } else full.ff = fixed
  mf = lme(full.ff, random=random, data = data) # full model
  null.ff = as.formula(paste(fixed[[2]],fixed[[1]],"1"))
  m0 = lme(null.ff, random=random, data = data) # null model
  Qmf = Qm(mf, method=method) # Qm for the full model
  Qm0 = Qm(m0, method=method) # Qm for the null model
  flag=FALSE
  if(missing(cn)) cn=sqrt(n)
  
  all.fixed.models = ASM(k.full-1,intcpt=TRUE)
  rsums = apply(all.fixed.models,1,sum)
  ms=NULL
  mframe=NULL
  
  # Null model
  cat(paste("Null model \n"))
  hatsigMM = sigMM(k.mod=1, method, k.full=k.full)
  UB = Qmf + cn*hatsigMM
  if(Qm0<=UB){
    cat(paste("hatQm:", round(Qm0,2),"; Upper bound:", round(UB,2)),"\n")
    cat(paste("hatQm <= UB:",Qm0<=UB,"\n"))
    cat(deparse(null.ff), "with random component", deparse(random))
    cat("\n")
    flag = TRUE
  }
  if(flag==TRUE & exhaustive==FALSE) return(invisible())
  # Remaining possibilities
  for(i in 2:k.full){
    ms = matrix(all.fixed.models[rsums==i,],ncol=k.full)
    cat(paste("Model size:",i,"\n"))
    hatsigMM = sigMM(k.mod=i, method, k.full=k.full)
    UB = Qmf + cn*hatsigMM
    for(j in 1:dim(ms)[1]){
      mframe = data.frame(X[,which(ms[j,]==1)])
      #names(mframe) = colnames(X)[which(ms[j,]==1)]
      ff = paste(names(X)[1]," ~ ",paste(names(mframe)[-1],collapse="+"),sep="")
      ff = as.formula(ff)
      em = lme(fixed=ff, random=random, data=mframe)
      hatQm = Qm(em,method=method)
      if(hatQm<=UB){
        cat(paste("hatQm:", round(hatQm,2),"; Upper bound:", round(UB,2)),"\n")
        cat(paste("hatQm <= UB:",hatQm<=UB,"\n"))
        cat(deparse(ff), "with random component", deparse(random))
        cat("\n")
        flag = TRUE
      } #else cat("No candidate models \n")
    }
    if(flag==TRUE & exhaustive==FALSE) return(invisible())
  }
}