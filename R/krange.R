krange = function(mfstar, m0, n, lower=null.ff,
                  upper=full.mod,data = Xstar){
  # backwards model selection using BIC
  bwards = step(mfstar, scope = list(lower=lower, upper=upper),
                direction="backward", k=log(n), trace=0)
  k.min = max(length(bwards$coef)-1,1)
  # forwards model selection using AIC
  fwards = step(m0, scope = list(lower=lower, upper=upper),
                direction="backward", k=2, trace=0)
  k.max = min(length(bwards$coef)+1,k.full)
  # Validata k.min and k.max values realative to one another to make sure
  # that min is (strictly) smaller than max
  # if to bwards BIC and fwds BIC then pick min(-1)
  # and do bwards AIC and fwds AIC then pick max(+1)
  # pass the results of these initial checks through to print or summary
  # especially the backwards BIC to give a conservative comparison to the fence
  return(list(k.max=k.max,k.min=k.min))
}