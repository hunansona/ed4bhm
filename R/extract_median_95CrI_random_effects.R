extract_median_95CrI_random_effects <-
function(inla.object) {
  #INPUT: an inla object
  #OUTPUT: 
  #takes an inla object as input, checks whether the random effects terms are available and extracts the random effects parameter means and sds
  #a matrix of all the random effects parameters in rows and means and sds in columns (ncol = 2)
  random.matrix <- NULL
  if (length(inla.object$summary.random) == 1) {
    # if the model contains 1 random effects component, that is, if the list inla.object$summary.random is 1
    random.length <- length(inla.object$summary.random)
    namere <- names(inla.object$summary.random)  #name of the random effect parameter
    
    random.matrix <- rbind(random.matrix, cbind(unlist(inla.object$summary.random[[1]][, "0.025quant"], use.names = TRUE), 
                                                unlist(inla.object$summary.random[[1]][, "0.5quant"], use.names = TRUE), unlist(inla.object$summary.random[[1]][, 
                                                                                                                                                                "0.975quant"], use.names = TRUE)))
    
    nre <- nrow(inla.object$summary.random[[1]])  #number of random effects terms
    
    row.names(random.matrix) <- paste(namere[1], 1:nre, sep = "")
  } else if (length(inla.object$summary.random) >= 2) {
    # if the model contains 2 or more random effects component, that is, if the list inla.object$summary.random is >= 2
    for (ii in 2:random.length) {
      random.matrix.i <- cbind(unlist(inla.object$summary.random[[ii]][, "0.025quant"]), unlist(inla.object$summary.random[[ii]][, 
                                                                                                                                 "0.5quant"]), unlist(inla.object$summary.random[[ii]][, "0.975quant"], use.names = TRUE))
      
      nre <- nrow(inla.object$summary.random[[ii]])  #number of random effects terms
      
      row.names(random.matrix.i) <- paste(namere[ii], 1:nre, sep = "")
      
      random.matrix <- rbind(random.matrix, random.matrix.i)
    }
  }
  # else { cat('The model doesn't contain random effect terms') }
  
  return(random.matrix)
}
