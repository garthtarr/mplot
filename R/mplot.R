#' Model selection and stability curves
#' 
#' Opens a shiny GUI to investigate a range of model selection 
#' and stability issues
#' 
#' 
#' @param mf a fitted model.
#' @param vis (optional) an object of type vis created using the
#'   vis() function.
#' @param af (optional) an af object of
#' @references Müller and Welsh (2010); Murray, Heritier and Müller (2013)
#' @export

mplot = function(mf,vis=NULL,af=NULL,B=100,...){
  if(!require(shiny)){
    install.packages("shiny")
  }
  # make the data globally accessible so shiny can access it
  # this may not be necessary, but I don't know how to pass
  # a (local) data frame through to the shiny server
  full.model <<- mf
  shiny.data.in <<- model.frame(mf)
  if(class(af)=="af"){
    af.res <<- af
  } else if(is.null(af)) {
    if(class(mf)=="lm"){
      cat("Adaptive fence in progress... \n")
      af.res <<- af(mf, n.cores = detectCores())
    } else {
      af.res <<- NULL
    }
  }
  if(class(vis)=="vis"){
    vis.res <<- vis
  } else if(is.null(vis)){
    if(class(mf)=="lm"){
      cat("Boostrapping for the model selection plots...")
      vis.res = vis(mf,B=B,nvmax = af.res$k.range$k.max)
    } else vis.res <<- NULL
  }
  yname <<- deparse(formula(mf)[[2]])
  shiny::runApp(system.file('mplot', package='mplot'))
}


