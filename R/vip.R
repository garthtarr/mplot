# ## variable inclusion plot
# 
# The VIP  visualises inclusion probabilities
# as a function of the penalty multiplier lambda.
# For each variable x_j subject to selection
# the proportion of times this variable is retained
# in the B final selected bootstrapped model
# is plotted for a range of lambda values, for 
# example lambda in [0,2log(n)].  More specifically,
# we calculate for each bootstrap sample b=1,...,B
# and

vip = function(fixed,data,family,nvmax,lambda.max){
  yname = deparse(fixed[[2]])
  if(missing(family)){
    mf = lm(fixed,data=data)
  } else {
    mf = glm(fixed,data=data,family=family) 
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
  n <- nrow(X)
  k.full <- length(mf$coef) 
  
  if(missing(nvmax)) nvmax = length(mf$coef)
  
  ## iterate over all possible models
  res.names= list()
  res.names[[1]] = "1"
  for(i in 2:nvmax){ # runs over the different model sizes
    var.name.mat = combn(names(mf$coef)[-1],i-1)
    res.names[[i]] = var.name.mat
  }
  ### look up table to see which variables are in the 'best' model
  res.names.t = lapply(res.names,t)
  nrows <- unlist(lapply(res.names.t, nrow))
  ncols = unlist(lapply(res.names.t, ncol))
  res.names.full = matrix(NA,ncol=max(ncols),nrow=sum(nrows))
  res.names.full[1,1] = "1"
  k=2
  for(i in 2:length(nrows)){
    for(j in 1:nrows[i]){
      res.names.full[k,1:ncols[i]] = res.names.t[[i]][j,]
      k=k+1
    }
  }
  
  res = matrix(NA,nrow = nrow(res.names.full),ncol=B)
  res.2ll = list()
  res.ll.model = list()
  res.min.model.names = list()
  
  for(b in 1:B){ # runs over the number of replications
    wts = rexp(n=n,rate=1)
    ## null model
    ff = paste(yname," ~ 1")
    ff = as.formula(ff)
    k=1
    #if(missing(family)){
    em = lm(formula=ff, data=X,weights=wts)
    #} else {
    #  em = glm(formula=ff, data=X, family=family)
    #}
    res[k,b] = -2*as.numeric(logLik(em))
    res.names[[1]] = "1"
    res.min.model.names[[1]] = "y ~ 1"
    res.ll.model[[1]] = ff
    
    for(i in 2:nvmax){ # runs over the different model sizes
      var.name.mat = combn(names(mf$coef)[-1],i-1)
      ll=NA # still need this as usual
      ll.model=NA
      min.model.names = NA
      for(j in 1:dim(var.name.mat)[2]){ # runs over each model on a given size
        ff = paste(yname," ~ ",paste(var.name.mat[,j],collapse="+"),sep="")
        ll.model[j] = ff
        ff = as.formula(ff)
        if(missing(family)){
          em = lm(formula=ff, data=X,weights=wts)
        } else {
          em = glm(formula=ff, data=X, family=family,weights=wts)
        }
        hatQm = -2*as.numeric(logLik(em))
        k=k+1
        res[k,b] = hatQm 
      }
      min.model = which(ll==min(ll))
      min.model.names[b] = ll.model[min.model]
    }
    res.min.model.names[[i]] = min.model.names
    res.2ll[[i]] = ll
    res.ll.model[[i]] = ll.model
  }
  lngth = function(x){
    length(na.omit(x))
  }
  ks = apply(res.names.full,1,lngth)+1
  ks[1] = 1
  lambdas = seq(0,2*log(n),0.1)
  min.pos = matrix(NA,ncol=B,nrow=length(lambdas))
  for(i in 1:length(lambdas)){
    resl = res+lambdas[i]*ks
    min.pos[i,] = apply(resl,2,which.min)
  }
  
  
  seq.lng = lapply(res.2ll,length)
  res = list(lk=data.frame(LL= unlist(res.2ll),k=rep(1:length(res.2ll),seq.lng)),
             models = res.names.full)
  class(res) = "lvp"
  return(res)
  
}
