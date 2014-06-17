#' Model selection/stability curves
#' 
#' Opens a shiny GUI to investigate model selection/stability issues
#' 
#' 
#' 
#' @param data a data frame containing the variables in the full
#'   model.  Currently this is required.  Future development will
#'   look at the \code{lm} function to replicate functionality,
#'   whereby if \code{data} is missing or variables are not found 
#'   in \code{data}, the variables are taken from 
#'   \code{environment(formula)}, typically the
#'   environment from which \code{lmfence} is called.
#' @references Müller and Welsh (2010); Murray, Heritier and Müller (2013)
#' @export

msc = function(fixed,data,family,lvp,fence=FALSE,B=100,...){
  if(!require(shiny)){
    install.packages("shiny")
  }
  # make the data globally accessible so shiny can access it
  # this may not be necessary, but I don't know how to pass
  # a (local) data frame through to the shiny server
  shiny.data.in <<- data
  #fixed
  #family
  #fence
  if(class(fence)=="af"){
    af.res <<- fence
  } else if(fence){
    if(missing(family)){
      af.res <<- af(fixed=fixed, data=data, n.cores=3)
    } else if(!missing(family)){
      af.res <<- af(fixed=fixed, data=data, family=family ,n.cores=3)
    }
  } else if(!fence) {
    af.res <<- NULL
  }
  # run the fence to find "best" model
  # pass it through and identify it on the msc plots
  # run the lvp procedure and make it globally available
  # if there are lots of variables, use leaps first then
  # only output lvp results for dimensions up to a couple 
  # larger than leaps
  if(class(lvp)=="lvp"){
    lvp.res <<- lvp
  } else {
    lvp.res <<- lvp(fixed=fixed, data = data, family = family, B=B)
  }
  yname <<- deparse(fixed[[2]])
  if(!require(googleVis)) install.packages("googleVis")
  shiny::runApp(system.file('mplot', package='mplot'))
}


