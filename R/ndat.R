ndat <-
function(inla.object) {
  #INPUT: 
  #an inla object
  #OUTPUT: 
  #number of input data points
  #This function extracts the number of data points given the inla object
  if (is.data.frame(inla.object$.args$data)) {
    return(length(inla.object$.args$data[, 1]))
  } else if (is.list(inla.object$.args$data)) {
    return(length(inla.object$.args$data[[1]]))
    cat("Warning: this program assumes that the first object listed in the data list is the primary outcome")
  } else {
    cat("Warning: undefined data structure (should be either a data.frame or a list)")
  }
  
}
