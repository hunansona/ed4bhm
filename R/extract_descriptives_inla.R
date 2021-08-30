extract_descriptives_inla <-
function(inla.object, dz = 0.75, diff.logdens = 15) {
  #INPUT:
  ##inla.object is an inla object
  ##dz , diff.logdens are arguments for the function inla.hyperpar from the INLA package
  #OUTPUT:
  #this function takes an inla object and extracts the descriptive statistics (mean and standard deviation) of the model parameters
  
  descriptive.matrix <- NULL
  
  fixed.matrix <- extract_fixed_effects(inla.object)
  hyperpar.matrix <- extract_hyperparameters(inla.object, dz, diff.logdens)
  random.matrix <- extract_random_effects(inla.object)
  
  if (!is.null(fixed.matrix)) {
    descriptive.matrix <- fixed.matrix
  }
  if (!is.null(hyperpar.matrix)) {
    descriptive.matrix <- rbind(descriptive.matrix, hyperpar.matrix)
  }
  if (!is.null(random.matrix)) {
    descriptive.matrix <- rbind(descriptive.matrix, random.matrix)
  }
  
  colnames(descriptive.matrix) <- c("mean", "sd")
  namere <- names(inla.object$summary.random)
  
  return(descriptive.matrix)
}
