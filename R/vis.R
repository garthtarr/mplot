#' Model stability curves and variable inclusion plots
#' 
#' Calculates and provides the plot methods for standard
#' and bootstrap enhanced model stability curves (lvk and
#' boot) as well as variable inclusion plots (vip).
#' 
#' @param mf a fitted 'full' model, the result of a call
#'   to lm or glm (and in the future lme or lmer).
#' @param nvmax size of the largest model that can still be 
#'   considered as a viable candidate.
#' @param B number of bootstrap replications
#' @param lambda.max maximum penalty value for the vip plot, 
#'   defaults to 2*log(n)
#' @param n.cores number of cores to be used when parallel
#'   processing the bootstrap.
#' @param force.in the names of variables that should be forced
#'   into all estimated models. (Not yet implemented.)
#' @param ... further arguments (currently unused)
#' @details The result of this function is essentially just a
#'   list. The supplied plot method provides a way to visualise the
#'   results.  
#' @seealso \code{\link{plot.vis}}
#' @references Mueller, S. and Welsh, A. H. (2010), On Model 
#'   Selection Curves. International Statistical Review, 78:240-256. 
#'   doi: 10.1111/j.1751-5823.2010.00108.x
#'   
#'   Murray, K., Heritier, S. and Mueller, S. (2013), Graphical 
#'   tools for model selection in generalized linear models. 
#'   Statistics in Medicine, 32:4438-4451. doi: 10.1002/sim.5855
#' @export
#' @examples
#' n = 100
#' set.seed(11)
#' e = rnorm(n)
#' x1 = rnorm(n)
#' x2 = rnorm(n)
#' x3 = x1^2
#' x4 = x2^2
#' x5 = x1*x2
#' y = 1 + x1 + x2 + e
#' dat = data.frame(y,x1,x2,x3,x4,x5)
#' lm1 = lm(y~.,data=dat)
#' v1 = vis(lm1)
#' \dontrun{
#' plot(v1,highlight="x1")
#' }

vis=function(mf,nvmax,B=100,lambda.max,
             n.cores=2,force.in=NULL,screen=FALSE,redundant=TRUE,...){
  
  m = mplot:::mextract(mf,screen=screen,redundant=redundant) 
  fixed = m$fixed
  yname = m$yname
  family = m$family
  X = m$X
  kf = m$k
  n = m$n
  initial.weights = m$wts
  if(missing(nvmax)) nvmax = kf
  
  ### when complete enumeration is possible,
  ### do it otherwise use leaps/bestglm to 
  ### only store the optimal models
  if(kf < 10){
    ## iterate over all possible models
    res.names= list()
    res.names[[1]] = "1"
    for(i in 2:nvmax){ # runs over the different model sizes
      res.names[[i]] = combn(unlist(strsplit(as.character(fixed)[3],split = " + ",fixed = TRUE)),i-1)
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
    
    require(doParallel)
    if(missing(n.cores)) n.cores = max(detectCores()-1,1)
    cl.vis = makeCluster(n.cores)
    registerDoParallel(cl.vis)
    require(foreach)
    res.2ll.temp = foreach(i = 2:nvmax) %dopar% {
      ll=NA # still need this as usual
      ll.model=NA
      # run over each model of a given size
      for(j in 1:dim(res.names[[i]])[2]){ 
        ff = paste(yname," ~ ",
                   paste(res.names[[i]][,j],collapse="+"),sep="")
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
      ll
    }
    res.2ll = c(res.2ll,res.2ll.temp)
    stopCluster(cl.vis)
    #### BOOTSTRAPPING COMPONENT ####
    if(B>1){
      cl.visB = makeCluster(n.cores)
      registerDoParallel(cl.visB)
      res = foreach(b = 1:B, .combine = cbind) %dopar% {
        res.temp = rep(NA,nrow(res.names.full))
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
        res.temp[k] = -2*as.numeric(logLik(em)) 
        res.min.model.names[[1]] = "y ~ 1"
        # run over the different model sizes:
        for(i in 2:nvmax){ 
          # run over each model of a given size:
          for(j in 1:dim(res.names[[i]])[2]){ 
            ff = paste(yname," ~ ",
                       paste(res.names[[i]][,j],collapse="+"),sep="")
            ff = as.formula(ff)
            if(any(class(mf)=="glm")==TRUE){
              em = glm(formula=ff, data=X, family=family,weights=wts)
            } else {
              em = lm(formula=ff, data=X,weights=wts)
            }
            k=k+1
            res.temp[k] = -2*as.numeric(logLik(em))
          }
        }
        res.temp
      }
      stopCluster(cl.visB)
      lngth = function(x){
        length(na.omit(x))
      }
      ks = apply(res.names.full,1,lngth)+1
      ks[1] = 1
      ### Variable inclusion Plot Calculations
      if(missing(lambda.max)) lambda.max = 2*log(n)
      lambdas = seq(0,lambda.max,0.01)
      min.pos = matrix(NA,ncol=B,nrow=length(lambdas))
      for(i in 1:length(lambdas)){
        resl = res+lambdas[i]*ks
        min.pos[i,] = apply(resl,2,which.min)
      }
      #### lvk where bubbles reflect frequencey of choice
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
                     best.models=res.min.model.names,screen=screen,
                     min.pos = min.pos, lambdas=lambdas, n=n)
  } else {
    
  }
  class(res.final) = "vis"
  return(res.final)
}




#' Plot diagnostics for a vis object
#' 
#' A plot method to visualise the results of a \code{vis} object.
#' 
#' @param x \code{vis} object, the result of \code{\link{vis}}
#' @param highlight the name of a variable that will be highlighted.
#' @param classic logical.  If \code{classic=TRUE} a 
#'   base graphics plot is provided instead of a googleVis plot.
#'   Default is \code{classic=FALSE}.
#' @param html.only logical. Use \code{html.only=TRUE} when including
#'   interactive plots in markdown documents (this includes rpres files).
#' @param which a vector specifying the plots to be output.  Variable 
#'   inclusion plots \code{which="vip"}; description loss against model
#'   size \code{which="lvk"}; bootstrapped description loss against 
#'   model size \code{which="boot"}.
#' @param width Width of the googleVis chart canvas area, in pixels. 
#'   Default: 800.
#' @param height Height of the googleVis chart canvas area, in pixels. 
#'   Default: 400.
#' @param chartWidth googleVis chart area width.  
#'   A simple number is a value in pixels; 
#'   a string containing a number followed by \code{\%} is a percentage. 
#'   Default: \code{"60\%"}
#' @param chartHeight googleVis chart area height. 
#'   A simple number is a value in pixels; 
#'   a string containing a number followed by \code{\%} is a percentage. 
#'   Default: \code{"80\%"}
#' @param fontSize font size used in googleVis chart.  Default: 12.
#' @param left space at left of chart (pixels?).  Default: "50".
#' @param top space at top of chart (pixels?).  Default: "30".
#' @param axisTitlesPosition Where to place the googleVis axis titles, 
#'   compared to the chart area. Supported values:
#'   "in" - Draw the axis titles inside the the chart area.
#'   "out" - Draw the axis titles outside the chart area.
#'   "none" - Omit the axis titles.
#' @param dataOpacity The transparency of googleVis data points, 
#'   with 1.0 being completely opaque and 0.0 fully transparent. 
#' @param options a list to be passed to the googleVis function giving
#'   complete control over the output.  Specifying a value for 
#'   \code{options} overwrites all other plotting variables.
#' @param shiny logical. Used internally to facilitate proper display
#'   of plots within the mplot shiny user interface.  Use 
#'   \code{shiny=TRUE} when displaying output within a shiny interface.
#' @param backgroundColor The background colour for the main area 
#'   of the chart. A simple HTML color string, 
#'   for example: 'red' or '#00cc00'.  Default: 'null' (there's an 
#'   issue with GoogleCharts when setting 'transparent' related to the 
#'   zoom window sticking - once that's sorted out, the default
#'   will change back to 'transparent')
#' @param ... further arguments (currently unused)
#' @seealso \code{\link{vis}}
#' @references Mueller, S. and Welsh, A. H. (2010), On Model 
#'   Selection Curves. International Statistical Review, 78:240-256. 
#'   doi: 10.1111/j.1751-5823.2010.00108.x
#'   
#'   Murray, K., Heritier, S. and Mueller, S. (2013), Graphical 
#'   tools for model selection in generalized linear models. 
#'   Statistics in Medicine, 32:4438-4451. doi: 10.1002/sim.5855
#' @export
#' @examples
#' n = 100
#' set.seed(11)
#' e = rnorm(n)
#' x1 = rnorm(n)
#' x2 = rnorm(n)
#' x3 = x1^2
#' x4 = x2^2
#' x5 = x1*x2
#' y = 1 + x1 + x2 + e
#' dat = data.frame(y,x1,x2,x3,x4,x5)
#' lm1 = lm(y~.,data=dat)
#' v1 = vis(lm1)
#' \dontrun{
#' plot(v1,highlight="x1",which="lvk")
#' }

plot.vis = function(x,highlight,classic=FALSE,html.only=FALSE,
                    which=c("vip","lvk","boot"),
                    width=800,height=400,fontSize=12,
                    left=50,top=30,chartWidth="60%",chartHeight="80%",
                    axisTitlesPosition="out",dataOpacity=0.5,
                    options=NULL,shiny=FALSE,
                    backgroundColor = 'transparent',...){
  if(backgroundColor=="transparent"){
    backgroundColor = "{stroke:null, fill:'null', strokeSize: 0}"
  } else {
    backgroundColor = paste("{stroke:null, fill:'",backgroundColor,
                            "', strokeSize: 0}",sep="")
  }
  find.var = function(x,highlight){
    is.element(highlight,x)
  }
  no.highlight = FALSE  
  if(missing(highlight)){ # highlight best bivariate variable
    no.highlight = TRUE
    k2which = x$lk$k==2
    k2LL = x$lk[k2which,1]
    k2mods = x$models[k2which,1]
    highlight = k2mods[which.min(k2LL)]
  }
  if("lvk"%in%which){
    if(classic){
      if(no.highlight){
        # step through the explanatory variables
        vars = unique(na.omit(as.vector(x$model)))
        var.ident = NA
        for(i in 2:length(vars)){
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
               pch=1, cex=1.3,
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
        use.options=list(title=gvis.title,
                         fontSize = fontSize,
                         vAxis="{title:'-2*Log-likelihood'}",
                         hAxis=gvis.hAxis,
                         axisTitlesPosition=axisTitlesPosition,
                         chartArea=chartArea,
                         width=width, height=height,
                         dataOpacity=dataOpacity,
                         backgroundColor=backgroundColor,
                         series= "{0:{color: 'gray', visibleInLegend: true}, 1:{color: 'blue', visibleInLegend: true}}",
                         explorer= "{axis: 'vertical',  keepInBounds: true, maxZoomOut: 1, maxZoomIn: 0.01, actions: ['dragToZoom', 'rightClickToReset']}")
      } else {use.options = options}
      fplot = gvisScatterChart(data=dat,options=use.options)
      if(shiny){
        return(fplot)
      } else if(html.only){
        fplot
      } else {
        plot(fplot)
      }
    }
  } 
  if("boot"%in%which){
    if(is.null(x$best.models))
      stop("You need to run vis() with B>1")
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
      use.options=list(title=gvis.title,
                       fontSize = fontSize,
                       vAxis="{title:'-2*Log-likelihood'}",
                       hAxis=gvis.hAxis,
                       sizeAxis = "{minValue: 0, minSize: 1,  
                                maxSize: 20, maxValue:1}",
                       axisTitlesPosition=axisTitlesPosition,
                       bubble = bubble,
                       chartArea=chartArea,
                       width=width, height=height,
                       backgroundColor=backgroundColor,
                       explorer= "{axis: 'vertical',  
                               keepInBounds: true,
                               maxZoomOut: 1,
                               maxZoomIn: 0.01,
                               actions: ['dragToZoom', 
                                         'rightClickToReset']}")
    } else {use.options = options}
    fplot = gvisBubbleChart(data=dat,idvar = "mods",xvar = "k",
                            yvar = "LL", colorvar = "var.ident", 
                            sizevar = "prob",
                            options=use.options)
    if(shiny){
      return(fplot)
    } else if(html.only){
      fplot
    } else {
      plot(fplot)
    } 
  }
  if("vip"%in%which){ # variable inclusion plot
    var.names = names(table(unlist(x$models)))
    var.names = var.names[var.names!="1"] # remove the intercept
    B = dim(x$min.pos)[2]
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
                      visibleInLegend: false}]"
    chartArea = paste("{left:",left,
                      ",top:",top,
                      ",width:'",chartWidth,
                      "',height:'",chartHeight,"'}",sep="")
    if(is.null(options)){
      use.options=list(title=gvis.title,
                       fontSize = fontSize,
                       vAxis="{title:'Bootstrapped probability'}",
                       hAxis="{title:'Penalty'}",
                       sizeAxis = "{minValue: 0, minSize: 1,  
                                maxSize: 20, maxValue:1}",
                       axisTitlesPosition=axisTitlesPosition,
                       series = lineseries,
                       chartArea=chartArea,
                       width=width, height=height,
                       backgroundColor=backgroundColor,
                       annotations = "{style:'line'}",
                       explorer= "{axis: 'vertical',  
                               keepInBounds: true,
                               maxZoomOut: 1,
                               maxZoomIn: 0.01,
                               actions: ['dragToZoom', 'rightClickToReset']}")
    } else {use.options = options}
    fplot = gvisLineChart(data=vip.df,
                          xvar="lambda",
                          yvar=c("AIC","AIC.annotation",
                                 "BIC","BIC.annotation",
                                 sortnames),
                          options=use.options)
    if(shiny){
      return(fplot)
    } else if(html.only){
      return(fplot)
    } else {
      return(plot(fplot))
    }
  } else return(invisible())
}
