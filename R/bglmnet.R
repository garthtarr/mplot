#' Boostrap model selection plots for glmnet
#' 
#' Experimental!!
#' 
#' @param mf a fitted 'full' model, the result of a call
#'   to lm or glm (and in the future lme or lmer).
#' @param nlambda how many penalty values to consider.
#' @param B number of bootstrap replications
#' @param n.cores number of cores to be used when parallel
#'   processing the bootstrap (Not yet implemented.)
#' @param force.in the names of variables that should be forced
#'   into all estimated models. (Not yet implemented.)
#' @param ... further arguments (currently unused)
#' @details The result of this function is essentially just a
#'   list. The supplied plot method provides a way to visualise the
#'   results.  
#' @seealso \code{\link{plot.bglmnet}}
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
#' bgn1 = mplot:::bglmnet(lm1)
#' \dontrun{
#' plot(bgn1,highlight="x1")
#' }

bglmnet = function (mf, nlambda = NULL, lambda=seq(0.05,0.95,0.05), B=100, 
                 probaseuil=1, penalty.factor, random, screen=FALSE) 
{
  require(glmnet)
  m = mextract(mf)
  fixed = m$fixed
  yname = m$yname
  family = m$family
  Xy = m$X
  kf = m$k
  X = Xy[,1:kf]
  Y = Xy[,(kf+1)]
  n = m$n
  X = scale(X) * sqrt(n)/sqrt(n - 1)
  #X[which(is.na(X))] = 0
  X = cbind(1, X)
  if (missing(penalty.factor)) {
    # link this with force.in
    penalty.factor = c(0, rep(1, kf+1))
  }
  if(!is.null(lambda)){
    nlambda = length(lambda)
  }
  temp = glmnet(X, Y, alpha = 1, nlambda = nlambda, 
                  lambda = lambda, 
                  penalty.factor = penalty.factor)
  mat = NULL
  # redefine lambda explicitly
  lambda = temp$lambda
  nlambda = length(lambda)
  compteur = matrix(0, kf+1, nlambda)
  mfstar = glm(fixed, data = Xy, family=family) 
  ystar = simulate(object=mfstar, nsim=B)
  #ystar[is.na(ystar)] = Xy[is.na(ystar),yname] 
  fam = family$family
  betaboot = array(0,dim = c(kf+1,nlambda,B))
  rownames(betaboot) = names(mfstar$coef)
  for (j in 1:B) {
    for (i in 1:nlambda) {
      temp = glmnet(X, ystar[,j], alpha = 1, 
                      lambda = lambda[i], 
                      #penalty.factor = penalty.factor,
                      family=fam)
      betaboot[,i,j] = (temp$beta[, 1] != 0)
    }
  }
  compteur2 = apply(betaboot,c(1,2),sum)
  probavariable = compteur2/B
  mods=list()
  get.names = function(x) paste(names(x)[x==1],collapse="+")
  for(k in 1:length(lambda)){
    mods[[k]] = table(apply(betaboot[,k,],2,get.names))
  }
  all.mods = unique(names(unlist(mods)))
  all.mods[all.mods==""]="1"
  all.ll = rep(0,length(all.mods))
  all.k = rep(0,length(all.mods))
  for(k in 1:length(all.mods)){
    # don't need to do this for models that inclide REDUNDANT.VARIABLE
    all.ll[k] = -2*logLik(glm(as.formula(paste(yname,"~",all.mods[k])),
                              data = Xy,family=family))
    # number of variables including intercept
    all.k[k] = length(unlist(strsplit(all.mods[[k]],
                                      split="+",fixed = TRUE)))+1 
  }
  all.k[all.mods=="1"] = 1
  colnames(probavariable) = round(lambda,3)
  mod.sum = data.frame(mod.names = all.mods,ll=all.ll,k=all.k)
  blarout = list(frequency = probavariable, 
                 lambda = lambda,
                 mods = mods,
                 mod.sum = mod.sum,
                 vars = names(mf$coef),
                 call = match.call())
  class(blarout) = "bglmnet"
  return(blarout)
}


#' Plot diagnostics for a bglmnet object
#' 
#' A plot method to visualise the results of a \code{bglmnet} object.
#' 
#' @param x \code{bglmnet} object, the result of \code{\link{bglmnet}}
#' @param highlight the name of a variable that will be highlighted.
#' @param classic logical.  If \code{classic=TRUE} a 
#'   base graphics plot is provided instead of a googleVis plot.
#'   Default is \code{classic=FALSE}.
#' @param html.only logical. Use \code{html.only=TRUE} when including
#'   interactive plots in markdown documents (this includes rpres files).
#' @param which a vector specifying the plots to be output. Variable 
#'   inclusion type plots \code{which="variables"} or model description loss against 
#'   penalty parameter \code{which="models"}.
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
#'   for example: 'red' or '#00cc00'.  Default: 'transparent'
#' @param plb lower bound on the probability of a model being selected. If
#'   a model has a selection probability lower than plb it will not be 
#'   plotted.
#' @param ... further arguments (currently unused)
#' @seealso \code{\link{bglmnet}}
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
#' bgn1 = mplot:::bglmnet(lm1)
#' \dontrun{
#' plot(bgn1,highlight="x1")
#' }

plot.bglmnet = function(x,highlight,classic=FALSE,html.only=FALSE,
                     which=c("models","variables"),
                     width=800,height=400,fontSize=12,
                     left=50,top=30,chartWidth="60%",chartHeight="80%",
                     axisTitlesPosition="out",dataOpacity=0.5,
                     options=NULL,shiny=FALSE,
                     backgroundColor = 'transparent',plb=0.01,...){
  B = sum(x$mods[[1]])
  
  if("models"%in%which){
    l.vec = rep(x$lambda,times = lapply(x$mods,length))
    mod.vec = unlist(x$mods)
    mod.names = names(mod.vec)
    mod.names[mod.names==""] = "1"
    mod.vec.counts = as.numeric(mod.vec)
    mod.vec.prob = mod.vec.counts/B
    df.temp = data.frame(l.vec,mod.vec.counts,mod.vec.prob,mod.names)
    # remove redundant variables
    df = df.temp[-grep("REDUNDANT.VARIABLE",df.temp$mod.names),]
    df.full = merge(df,x$mod.sum,all.x = TRUE)
    df.sub = subset(df.full,df.full$mod.vec.prob>plb)
    df.sub$mod.names = as.character(df.sub$mod.names)
    if(classic){
      warning("Classic plot not implemented.")
      #plot(df.sub$ll~df.sub$l.vec,cex=df.sub$mod.vec.counts/5,
      #     xlim=c(min(x$lambda),max(x$lambda)))
    }
    if(missing(highlight)){ # highlight best bivariate variable
      no.highlight = TRUE
      if(sum(df.sub$k==2)>0){
      dfk2 = unique(df.sub[df.sub$k==2,c(1,5)])
      highlight = dfk2$mod.names[which.min(dfk2$ll)]
      } else highlight =  x$vars[2]
    }
    
    mod.parts = lapply(df.sub$mod.names,FUN = strsplit,"+",fixed=TRUE)
    find.var = function(x,highlight){
      is.element(highlight,unlist(x))
    }
    var.ident = unlist(lapply(mod.parts,find.var,highlight=highlight))
    var.ident[var.ident==TRUE] =  paste("With",highlight)
    var.ident[var.ident==FALSE] =  paste("Without",highlight)
    df.sub$var.ident = var.ident
    gvis.title = paste("Description loss against penalty parameter",sep="")
    #x.ticks=paste(1:max(x$lk$k),collapse=",") 
    gvis.hAxis = paste("{title:'Penalty parameter',
                   maxValue:",max(x$lambda)+0.05," ,
                   minValue:",min(x$lambda)-0.05,"}")
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
    
    fplot = gvisBubbleChart(data=df.sub,idvar = "mod.names",xvar = "l.vec",
                            yvar = "ll", colorvar = "var.ident",
                            sizevar = "mod.vec.prob",
                            options=use.options)
    if(shiny){
      return(fplot)
    } else if(html.only){
      fplot
    } else {
      plot(fplot)
    }
  } 
  if("variables"%in%which){
    var.names = x$vars[x$vars!="(Intercept)"]
    p.var = t(x$freq)
    p.var = p.var[,colnames(p.var)%in%var.names]
    sortnames = names(sort(apply(p.var,2,mean),decreasing=TRUE))
    vip.df = p.var[,sortnames]
    vip.df = data.frame(lambda=x$lambda,vip.df)
    #tid = c(1,2,4,6:dim(vip.df)[2])
    #vip.df[, tid] = sapply(vip.df[, tid], as.numeric)
    gvis.title = "Variable inclusion plot"
    chartArea = paste("{left:",left,
                      ",top:",top,
                      ",width:'",chartWidth,
                      "',height:'",chartHeight,"'}",sep="")
    if(is.null(options)){
      use.options=list(title=gvis.title,
                       fontSize = fontSize,
                       vAxis="{title:'Bootstrapped probability'}",
                       hAxis="{title:'Penalty parameter'}",
                       sizeAxis = "{minValue: 0, minSize: 1,  
                       maxSize: 20, maxValue:1}",
                       axisTitlesPosition=axisTitlesPosition,
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
                          yvar=sortnames,
                          options=use.options)
    if(shiny){
      return(fplot)
    } else if(html.only){
      return(fplot)
    } else {
      return(plot(fplot))
    }
  }
}

