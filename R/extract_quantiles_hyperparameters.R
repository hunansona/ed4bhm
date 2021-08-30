extract_quantiles_hyperparameters <-
function(inla.object, dz = 0.75 , diff.logdens = 15 ) {
  #INPUT
  ##inla.object is an inla object
  ##dz, diff.logdens are arguments for the function inla.hyperpar from the INLA package
  #OUTPUT:
  ##takes an inla object as input, checks whether the hyperparameters are available and extracts the hyperparameters means and sds
  #a matrix of all the hyperparameters in rows and means and sds in columns (ncol = 2)
  #WARNING: only the INLA internal scale (log) is considered and extracted (for example, the mean and sd of the log precision is extracted)
  
  hyperpar.matrix <- NULL
  
  if (!is.null(inla.object$summary.hyperpar)) {
    # if the model contains hyperparameters component, that is, if inla.object$summary.hyperpar is not NULL
    
    # inla.object.hyp <- inla.hyperpar(inla.object, dz , diff.logdens)#, verbose = FALSE changes the result
    
    hyps <- inla.object$internal.summary.hyperpar  #inla.object.hyp$internal.summary.hyperpar
    
    hyperpar.list <- cbind(hyps["0.025quant"], hyps["0.5quant"], hyps["0.975quant"])
    
    hyperpar.matrix <- matrix(unlist(hyperpar.list), ncol = 3)  #ncol = 2 for 95CrI and median
    
    row.names(hyperpar.matrix) <- row.names(hyps)
    
  }
  return(hyperpar.matrix)
}
