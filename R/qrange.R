#' Identify an appropriate rance of values over which to bootstrap
#' 
#' This function takes an upper and lower dimension size (obtained by
#' forwards and backwards model selection and then adding and subtracting
#' 1 from each of the extremes to encompas a broader range of models).
#' For both the small and large model size, the "best" model is identified
#' using the \code{leaps} package and the corresponding lack of fit measure
#' is calculated.
qrange = function(k.range,yname,fixed,data,method){
  candidate.models = summary(regsubsets(x = fixed, 
                                        data = data, 
                                        nbest = 1,
                                        nvmax = k.range$k.max-1))$which+0
  
  small.row = which(rowSums(candidate.models)==k.range$k.min)
  small.names = colnames(candidate.models)[which(candidate.models[small.row,]==1)]
  if(length(small.names)>1){
    small.ff = paste(yname," ~ ",paste(small.names[-1],collapse="+"),sep="")
  } else small.ff = paste(yname,"~1")  
  small.ff = as.formula(small.ff)
  small.em = lm(small.ff, data=data)
  
  big.row = which(rowSums(candidate.models)==k.range$k.max)
  big.names = colnames(candidate.models)[which(candidate.models[big.row,]==1)]
  if(length(big.names)>1){
    big.ff = paste(yname," ~ ",paste(big.names[-1],collapse="+"),sep="")
  } else big.ff = paste(yname,"~1")  
  big.ff = as.formula(big.ff)
  big.em = lm(big.ff, data=data)
  
  Q.min = Qm(big.em,method=method)
  Q.max = Qm(small.em,method=method)
  return(list(Q.min=Q.min,Q.max=Q.max))
}