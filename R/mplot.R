#' Model selection and stability curves
#' 
#' Opens a shiny GUI to investigate a range of model selection 
#' and stability issues
#' 
#' 
#' @param mf a fitted model.
#' @param vis (optional) an object of type vis created using the
#'   vis() function.
#' @param af (optional) an object of type af created using the af()
#'   function.
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
#' v1 = vis(lm1,n.cores=3)
#' af1 = af(lm1,n.cores=3)
#' mplot(lm1,vis=v1,af=af1)

mplot = function(mf,vis=NULL,af=NULL,B=100){
  if(!require(shiny)){
    install.packages("shiny")
    require(shiny)
  }
  # make the data globally accessible so shiny can access it
  # this may not be necessary, but I don't know how to pass
  # a (local) data frame through to the shiny server
  full.model <<- mf
  shiny.data.in <<- model.frame(mf)
  if(class(af)=="af"){
    af.res <<- af
  } else {
    af.res <<- NULL
  }
  #else if(is.null(af)) { could run automatically?
  #  if(class(mf)=="lm"){
  #    cat("Adaptive fence in progress... \n")
  #    af.res <<- af(mf, n.cores = detectCores())
  #  } 
  #}
  if(class(vis)=="vis"){
    lvp.res <<- vis
  } #else if(is.null(vis)){ could run automatically?
  #  if(class(mf)=="lm"){
  #    cat("Boostrapping for the model selection plots...")
  #    lvp.res <<- vis(mf,B=B,nvmax = af.res$k.range$k.max)
  #  } else lvp.res <<- NULL
  #}
  yname <<- deparse(formula(mf)[[2]])
  shiny::runApp(system.file('mplot', package='mplot'))
}


