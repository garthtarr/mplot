#' Model selection/stability curves and variable importance
#' 
#' Performs the heavy lifting and provides the plot methods for lvk, 
#' boot and vip
#' 
#' @param mf the full fitted model of type lm or glm
#' @param nvmax the max number of variables to consider as being important
#' @param B the number of bootstrap replications
#' @param lambda.max the maximum penalty value for the vip plot, 
#'   defaults to 2*log(n)
#' @references Müller and Welsh (2010); Murray, Heritier and Müller (2013)
#' @export

vis=function(mf,nvmax,B=100,lambda.max,...){
  fixed = formula(mf)
  yname = deparse(fixed[[2]])
  # using this approach to cope when there are indicator
  # variables in the formula, they get spelled out in
  # model.matrix
  X = model.matrix(mf)
  if(colnames(X)[1]=="(Intercept)"){
    # overwrite intercept with y-variable
    X[,1] = model.frame(mf)[,yname] 
    no.int = FALSE
  } else {
    X = cbind(model.frame(mf)[,yname],X)
  }  
  colnames(X)[1] = yname
  X = data.frame(X)
  n <- nrow(X)
  k.full <- length(mf$coef) 
  
  if(missing(nvmax)) nvmax = length(mf$coef)
  family=family(mf)
  
  ## iterate over all possible models
  res.names= list()
  res.names[[1]] = "1"
  for(i in 2:nvmax){ # runs over the different model sizes
    res.names[[i]] = combn(names(mf$coef)[-1],i-1)
  }
  # create a look up table res.names.full which can be used
  # to see which variables are in the 'best' model
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
  
  #### SINGLE PASS OVER ALL MODELS ####
  ff = paste(yname," ~ 1")
  ff = as.formula(ff)
  if(any(class(mf)=="glm")==TRUE){
    em = glm(formula=ff, data=X, family=family)
  } else {
    em = lm(formula=ff, data=X)
  }
  k=1
  res.2ll[[1]] = -2*as.numeric(logLik(em))
  res.ll.model[[1]] = ff
  for(i in 2:nvmax){ # runs over the different model sizes
    var.name.mat = combn(names(mf$coef)[-1],i-1)
    ll=NA # still need this as usual
    ll.model=NA
    # run over each model of a given size
    for(j in 1:dim(var.name.mat)[2]){ 
      ff = paste(yname," ~ ",
                 paste(var.name.mat[,j],collapse="+"),sep="")
      ll.model[j] = ff
      ff = as.formula(ff)
      if(any(class(mf)=="glm")==TRUE){
        em = glm(formula=ff, data=X, family=family)
      } else {
        em = lm(formula=ff, data=X)
      }
      hatQm = -2*as.numeric(logLik(em))
      ll[j] = hatQm 
    }
    res.2ll[[i]] = ll
    res.ll.model[[i]] = ll.model
  }
  
  
  
  #### BOOTSTRAPPING COMPONENT ####
  if(B>1){
    for(b in 1:B){ # runs over the number of replications
      wts = rexp(n=n,rate=1)
      ## null model
      ff = paste(yname," ~ 1")
      ff = as.formula(ff)
      if(any(class(mf)=="glm")==TRUE){
        em = glm(formula=ff, data=X, family=family, weights=wts)
      } else {
        em = lm(formula=ff, data=X, weights=wts)
      }
      k=1
      res[k,b] = -2*as.numeric(logLik(em)) 
      res.min.model.names[[1]] = "y ~ 1"
      # run over the different model sizes:
      for(i in 2:nvmax){ 
        var.name.mat = combn(names(mf$coef)[-1],i-1)
        # run over each model of a given size:
        for(j in 1:dim(var.name.mat)[2]){ 
          ff = paste(yname," ~ ",
                     paste(var.name.mat[,j],collapse="+"),sep="")
          ff = as.formula(ff)
          if(any(class(mf)=="glm")==TRUE){
            em = glm(formula=ff, data=X, family=family,weights=wts)
          } else {
            em = lm(formula=ff, data=X,weights=wts)
          }
          k=k+1
          res[k,b] = -2*as.numeric(logLik(em))
        }
      }
    }
    lngth = function(x){
      length(na.omit(x))
    }
    ks = apply(res.names.full,1,lngth)+1
    ks[1] = 1
    
    ### Variable Importance Plot Calculations
    if(missing(lambda.max)) lambda.max = 2*log(n)
    lambdas = seq(0,lambda.max,0.01)
    min.pos = matrix(NA,ncol=B,nrow=length(lambdas))
    for(i in 1:length(lambdas)){
      resl = res+lambdas[i]*ks
      min.pos[i,] = apply(resl,2,which.min)
    }
    
    #### LvP where bubbles reflect frequencey of choice
    t1 = split(res,f=ks)
    t2 = lapply(t1,matrix,ncol=ncol(res))
    which.fn = function(x) which(x==min(x))
    which.fn1 = function(x) apply(x,2,which.fn)
    min.model = lapply(t2,which.fn1)
    t4 = split(res.names.full,f=ks)
    t5 = lapply(t4,matrix,ncol=ncol(res.names.full))
    for(i in 1:length(t5)){
      res.min.model.names[[i]] = t5[[i]][min.model[[i]],]
    }
  }
  seq.lng = lapply(res.2ll,length)
  res.final = list(lk=data.frame(LL= unlist(res.2ll),
                                 k=rep(1:length(res.2ll),seq.lng)),
                   models = res.names.full, 
                   best.models=res.min.model.names,
                   min.pos = min.pos, lambdas=lambdas, n=n)
  class(res.final) = "vis"
  return(res.final)
}




#' Plot diagnostics for an vis object
#' 
#' Summary plot of the vis results.
#' 
#' @param x \code{vis} object, the result of \code{\link{vis}}
#' @param highlight a vector indicating which variables 
#'   should be highlighted.
#' @param classic logical.  If \code{classic=TRUE} a 
#'   base graphics plot is provided instead of a googleVis plot.
#'   Default is \code{classic=FALSE}.
#' @param ... other parameters to be passed through to 
#'   plotting functions.
#' @export
# S3 method for class 'af'
plot.vis = function(x,highlight,classic=FALSE,html.only=FALSE,
                    which=c("vip","lvk","boot"),
                    width=800,height=400,fontSize=12,
                    left=50,top=30,chartWidth="60%",chartHeight="80%",
                    axisTitlesPosition="out",dataOpacity=0.5,...){
  find.var = function(x,highlight){
    is.element(highlight,x)
  }
  if(which=="lvk"){
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
          plot(x$lk$LL[!var.ident]~x$lk$k[!var.ident],
               bg=gray(0.1, alpha=0.5),
               pch=22, cex=1.3,
               xlab = "Number of parameters",
               ylab = "-2*Log-likelihood",
               ylim = c(min(x$lk$LL),max(x$lk$LL)),
               xlim = c(min(x$lk$k),max(x$lk$k)))
          points(x$lk$LL[var.ident]~x$lk$k[var.ident], 
                 pch=24, bg=colbg,
                 col = colfg, cex=1.2)
          legend("topright",legend=vars[i], col = colfg,
                 pt.bg=colbg, pch = 24)
        }
      } else {
        var.ident = matrix(NA,ncol=length(highlight),
                           nrow=nrow(x$models))
        iter = 1:length(highlight)
        for(i in iter){
          var.ident[,i] = apply(x$model, 1, find.var, 
                                highlight=highlight[i])
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
      chartArea = paste("{left:",left,
                        ",top:",top,
                        ",width:'",chartWidth,
                        "',height:'",chartHeight,"'}",sep="")
      if(is.null(options)){
        options=list(title=gvis.title,
                     vAxis="{title:'-2*Log-likelihood'}",
                     hAxis=gvis.hAxis,
                     axisTitlesPosition=axisTitlesPosition,
                     chartArea=chartArea,
                     width=width, height=height,
                     dataOpacity=dataOpacity,
                     series= "{0:{color: 'gray', 
                                  visibleInLegend: true},
                               1:{color: 'blue', 
                                  visibleInLegend: true}}",
                     explorer= "{axis: 'vertical',  
                                 keepInBounds: true,
                                 maxZoomOut: 1,
                                 maxZoomIn: 0.01,
                                 actions: ['dragToZoom', 
                                    'rightClickToReset']}")
      }
      fplot = gvisScatterChart(data=dat,options=options)
      if(html.only){
        return(fplot)
      } else {
        return(plot(fplot))
      }
    }
  } else if(which=="boot"){
    if(is.null(x$best.models))
      stop("You need to run vis() with B>1")
    suppressPackageStartupMessages(library(googleVis))
    var.ident = apply(x$model, 1, find.var, highlight=highlight)
    var.ident[var.ident==TRUE] = paste("With",highlight)
    var.ident[var.ident==FALSE] = paste("Without",highlight)
    mods = split(x$models, 1:NROW(x$models))
    mods = lapply(mods,na.omit)
    mods = lapply(mods,paste,collapse="+")
    mods = unlist(as.matrix(mods))
    pastena = function(x) paste(na.omit(x),collapse="+")
    collapse1 = function(x) apply(x,1,pastena)
    bms = lapply(x$best.models,collapse1)
    mod.ns = lapply(bms,FUN=length)
    mod.prob = lapply(bms,FUN=table)
    for(i in 1:length(mod.prob)){
      mod.prob[[i]] = mod.prob[[i]]/mod.ns[[i]]
    }
    mod.prob=unlist(mod.prob)
    mod.names = names(mod.prob)
    mod.prob = data.frame(mod.prob,mod.names)
    mods.df = data.frame(mod.names = mods)
    mods.df$id = 1:nrow(mods.df)
    mod.df = merge(x=mods.df,y=mod.prob,all=TRUE)
    mod.df = mod.df[order(mod.df$id), ]
    dat = data.frame(mods = mods,
                     k = x$lk$k, 
                     LL = x$lk$LL,
                     prob = mod.df$mod.prob,
                     var.ident = var.ident
    )
    gvis.title = paste("Description loss against k",sep="")
    x.ticks=paste(1:max(x$lk$k),collapse=",") 
    gvis.hAxis = paste("{title:'Number of parameters',
                       maxValue:",max(x$lk$k)+0.5," ,
                       minValue:",0.5," , 
                       ticks: [",x.ticks,"]}")
    chartArea = paste("{left:",left,
                      ",top:",top,
                      ",width:'",chartWidth,
                      "',height:'",chartHeight,"'}",sep="")
    bubble = paste("{opacity:",dataOpacity,
                   ", textStyle: {color: 'none'}}",sep="")
    if(is.null(options)){
      options=list(title=gvis.title,
                   vAxis="{title:'-2*Log-likelihood'}",
                   hAxis=gvis.hAxis,
                   sizeAxis = "{minValue: 0, minSize: 1,  
                                maxSize: 20, maxValue:1}",
                   axisTitlesPosition=axisTitlesPosition,
                   bubble = bubble,
                   chartArea=chartArea,
                   width=width, height=height,
                   explorer= "{axis: 'vertical',  
                               keepInBounds: true,
                               maxZoomOut: 1,
                               maxZoomIn: 0.01,
                               actions: ['dragToZoom', 
                                         'rightClickToReset']}")
    }
    fplot = gvisBubbleChart(data=dat,idvar = "mods",xvar = "k",
                            yvar = "LL", colorvar = "var.ident", 
                            sizevar = "prob",
                            options=options)
    if(html.only){
      return(fplot)
    } else {
      return(plot(fplot))
    } 
  } else if (which=="vip"){
    for(i in 1:length(x$lambdas)){
      ### VIP plot here!
      var.names = x$models[dim(x$models)[1],]
      B = dim(x$min.pos)[2]
      # first lambda value BS reps
      # turn in to a function and use apply
      p.var = matrix(NA,nrow=length(x$lambdas),ncol = length(var.names))
      colnames(p.var) = var.names
      for(i in 1:length(x$lambdas)){
        l1 = x$min.pos[i,] 
        selected.mods = x$models[l1,]
        selected.mods = factor(selected.mods,levels=var.names)
        p.var[i,] = table(selected.mods)/B
      }
      sortnames = names(sort(apply(p.var,2,mean),decreasing=TRUE))
      vip.df = p.var[,sortnames]
      vip.df = data.frame(lambda=x$lambdas,AIC=NA,AIC.annotation=NA,
                          BIC=NA,BIC.annotation=NA,vip.df)
      aicline = rbind(c(2,0,NA,NA,NA,rep(NA,length(var.names))),
                      c(2,1,"AIC",NA,NA,rep(NA,length(var.names))),
                      c(log(x$n),NA,NA, 0,NA,rep(NA,length(var.names))),
                      c(log(x$n),NA,NA,1,"BIC",rep(NA,length(var.names))))
      colnames(aicline) = colnames(vip.df)
      vip.df = rbind(vip.df,aicline)
      tid = c(1,2,4,6:dim(vip.df)[2])
      vip.df[, tid] = sapply(vip.df[, tid], as.numeric)
      gvis.title = "Variable inclusion plot"
      lineseries="[{lineDashStyle: [2,2], lineWidth: 2, color:'gray',
                      visibleInLegend: false},
                    {lineDashStyle: [2,2], lineWidth: 2, color:'gray',
                      visibleInLegend: false},
                   {lineDashStyle: [1], lineWidth: 5}, 
                   {lineDashStyle: [10, 10, 10, 10], lineWidth: 4},
                   {lineDashStyle: [5, 20, 10, 20, 5], lineWidth: 4}]"
      chartArea = paste("{left:",left,
                        ",top:",top,
                        ",width:'",width,
                        "',height:'",height,"'}",sep="")
      if(is.null(options)){
        options=list(title=gvis.title,
                     vAxis="{title:'Bootstrapped probability'}",
                     hAxis="{title:'λ'}",
                     sizeAxis = "{minValue: 0, minSize: 1,  
                                  maxSize: 20, maxValue: 1}",
                     axisTitlesPosition=axisTitlesPosition,
                     series = lineseries,
                     chartArea=chartArea,
                     width=width, height=400,
                     annotations = "{style:'line'}",
                     explorer= "{axis: 'vertical',  
                                 keepInBounds: true,
                                 maxZoomOut: 1,
                                 maxZoomIn: 0.01,
                                 actions: ['dragToZoom', 
                                           'rightClickToReset']}")
      }
      fplot = gvisLineChart(data=vip.df,
                            xvar="lambda",
                            yvar=c("AIC","AIC.annotation",
                                   "BIC","BIC.annotation",
                                   sortnames),
                            options=options)
      if(html.only){
        return(fplot)
      } else {
        return(plot(fplot))
      } 
    }
  }
}
