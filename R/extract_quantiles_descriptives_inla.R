extract_quantiles_descriptives_inla <-
function(inla.object, dz, diff.logdens) {
  #INPUT:
  ##inla.object is an inla object
  ##dz , diff.logdens are arguments for the function inla.hyperpar from the INLA package
  #OUTPUT:
  #this function takes an inla object and extracts the descriptive statistics (mean and standard deviation) of the model parameters
  
  descriptive.matrix <- NULL
  
  fixed.matrix <- extract_quantiles_fixed_effects(inla.object)
  hyperpar.matrix <- extract_quantiles_hyperpars(inla.object, dz, diff.logdens)
  random.matrix <- extract_quantiles_random_effects(inla.object)
  
  if (!is.null(fixed.matrix)) {
    descriptive.matrix <- fixed.matrix
  }
  if (!is.null(hyperpar.matrix)) {
    descriptive.matrix <- rbind(descriptive.matrix, hyperpar.matrix)
  }
  if (!is.null(random.matrix)) {
    descriptive.matrix <- rbind(descriptive.matrix, random.matrix)
  }
  
  colnames(descriptive.matrix) <- c("0.025quant", "0.5quant", "0.975quant")
  namere <- names(inla.object$summary.random)
  
  return(descriptive.matrix)
}
