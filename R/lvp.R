lvp=function(fixed,data,family,nvmax,...){
  
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
  res=NA
  res.2ll = list()
  res.names = list()
  res.ll.model = list()
  
  ## null model
  ff = paste(yname," ~ 1")
  ff = as.formula(ff)
  if(missing(family)){
    em = lm(formula=ff, data=X)
  } else {
    em = glm(formula=ff, data=X, family=family)
  }
  res.2ll[[1]] = -2*as.numeric(logLik(em))
  res.names[[1]] = "1"
  res.ll.model[[1]] = ff
  for(i in 2:nvmax){
    var.name.mat = combn(names(mf$coef)[-1],i-1)
    ll=NA
    ll.model=NA
    for(j in 1:dim(var.name.mat)[2]){
      ff = paste(yname," ~ ",paste(var.name.mat[,j],collapse="+"),sep="")
      ll.model[j] = ff
      ff = as.formula(ff)
      if(missing(family)){
        em = lm(formula=ff, data=X)
      } else {
        em = glm(formula=ff, data=X, family=family)
      }
      
      hatQm = -2*as.numeric(logLik(em))
      ll[j] = hatQm    
    }
    res.2ll[[i]] = ll
    res.names[[i]] = var.name.mat
    res.ll.model[[i]] = ll.model
  }
  
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
  
  seq.lng = lapply(res.2ll,length)
  res = list(lk=data.frame(LL= unlist(res.2ll),k=rep(1:length(res.2ll),seq.lng)),
             models = res.names.full)
  class(res) = "lvp"
  return(res)
  
}




#' Plot diagnostics for an lvp object
#' 
#' Summary plot of the lvp results.
#' 
#' @param x \code{lvp} object, the result of \code{\link{lvp}}
#' @param highlight a vector indicating which variables should be highlighted.
#' @param classic logical.  If \code{classic=TRUE} a 
#'   base graphics plot is provided instead of a googleVis plot.
#'   Default is \code{classic=FALSE}.
#' @param ... other parameters to be passed through to 
#'   plotting functions.
#' @export
# S3 method for class 'af'
plot.lvp = function(x,highlight,classic=FALSE,html.only=FALSE,...){
  find.var = function(x,highlight){
    is.element(highlight,x)
  }
  if(!require(googleVis)|classic){
    if(missing(highlight)){
      # step through the explanatory variables
      vars = unique(na.omit(as.vector(x$model)))
      var.ident = NA
      for(i in 1:length(vars)){
        if(i %% 2 == 0){
          colbg=rgb(1,0,0,0.5)
          colfg=rgb(1,0,0)
        } else{
          colbg=rgb(0,0,1,0.5)
          colfg=rgb(0,0,1)
        }
        var.ident = apply(x$model, 1, find.var, highlight=vars[i])
        par(ask=TRUE)
        plot(x$lk$LL[!var.ident]~x$lk$k[!var.ident],bg=gray(0.1, alpha=0.5),
             pch=22, cex=1.3,
             xlab = "Number of parameters",
             ylab = "-2*Log-likelihood",
             ylim = c(min(x$lk$LL),max(x$lk$LL)),
             xlim = c(min(x$lk$k),max(x$lk$k)))
        points(x$lk$LL[var.ident]~x$lk$k[var.ident], pch=24, bg=colbg,
               col = colfg, cex=1.2)
        legend("topright",legend=vars[i], col = colfg,
               pt.bg=colbg, pch = 24)
      }
    } else {
      var.ident = matrix(NA,ncol=length(highlight),nrow=nrow(x$models))
      iter = 1:length(highlight)
      for(i in iter){
        var.ident[,i] = apply(x$model, 1, find.var, highlight=highlight[i])
      }
      not.ident = rowSums(var.ident)==0
      pch = c(19,18,2,3,4,6:25)
      plot(x$lk$LL~x$lk$k,col=not.ident,pch=1,
           xlab = "Number of parameters",
           ylab = "-2*Log-likelihood")
      for(i in iter){
        points(x$lk$LL[var.ident[,i]]~x$lk$k[var.ident[,i]],
               col=i+1, pch=pch[i], cex = 1.5-0.1*i)
      }
      legend("bottomleft",legend=highlight,col = iter+1,
             pch = pch[iter])
    }
  } else {
    suppressPackageStartupMessages(library(googleVis))
    
    var.ident = apply(x$model, 1, find.var, highlight=highlight)
    var.ident.na = var.ident
    var.ident.na[var.ident.na==FALSE]=NA
    with.var = x$lk$LL*var.ident.na
    var.noident.na = 1-var.ident
    var.noident.na[var.noident.na==FALSE]=NA
    without.var = x$lk$LL*var.noident.na
    mods = split(x$models, 1:NROW(x$models))
    mods = lapply(mods,na.omit)
    mods = lapply(mods,paste,collapse="+")
    mods = unlist(as.matrix(mods))
    dat = data.frame(k = x$lk$k, 
                     without.var, without.var.html.tooltip=mods,
                     with.var, with.var.html.tooltip=mods)
    colnames(dat)[4] = paste("With",highlight)
    colnames(dat)[2] = paste("Without",highlight)
    gvis.title = paste("Description loss against k",sep="")
    x.ticks=paste(1:max(x$lk$k),collapse=",") 
    gvis.hAxis = paste("{title:'Number of parameters', ticks: [",
                       x.ticks,"]}")
    fplot = gvisScatterChart(data=dat,
                             options=list(title=gvis.title,
                                          vAxis="{title:'-2*Log-likelihood'}",
                                          hAxis=gvis.hAxis,
                                          axisTitlesPosition="out",
                                          chartArea="{left:50,top:30,width:'60%',height:'80%'}",
                                          width=800, height=400,
                                          dataOpacity=0.5,
                                          series= "{0:{color: 'gray', visibleInLegend: true},
                                          1:{color: 'blue', visibleInLegend: true}}",
                                          explorer= "{axis: 'vertical',  
                                          keepInBounds: true,
                                          maxZoomOut: 1,
                                          maxZoomIn: 0.01,
                                          actions: ['dragToZoom', 'rightClickToReset']}"))
    if(html.only){
      return(fplot)
    } else {
      return(plot(fplot))
    }
  }
}