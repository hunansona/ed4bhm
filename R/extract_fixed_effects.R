extract_fixed_effects <-
function(inla.object) {
  #INPUT: an inla object
  #OUTPUT: 
  #takes an inla object as input, checks whether the fixed effects terms are available and extracts the fixed effects parameter means and sds
  #a matrix of all the fixed effects parameters in rows and means and sds in columns (ncol = 2)
  fixed.matrix <- NULL
  if (nrow(inla.object$summary.fixed) != 0) {
    # if the model contains fixed effects component, that is, if the list inla.object$summary.fixed is not empty then extract
    # the descriptive statistics, otherwise see the warning message
    fixed.matrix <- cbind(unlist(inla.object$summary.fixed["mean"], use.names = TRUE), unlist(inla.object$summary.fixed["sd"], 
                                                                                              use.names = TRUE))
    row.names(fixed.matrix) <- row.names(inla.object$summary.fixed)
    
  }
  # else { cat('The model doesn't contain fixed effect terms') }
  return(fixed.matrix)
}
