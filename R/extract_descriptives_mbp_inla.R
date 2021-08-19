extract_descriptives_mbp_inla <-
function(inla.object.base, delta, dz, diff.logdens) {
  #extracts the descriptive statistics for (mbp) minus, base and plus models with extract_descriptives_inla()
  #INPUT:
  ##inla.object.base is the base model fitted in inla
  ##delta is the numerical differentiation step (w = 1+delta)
  ##log.scale if you need to extract the logarithmic scale of the hyperparameter precision (internal inla scale) 
  #OUTPUT:
  ##list composed by the descriptive matrix and the delta

  
  inla_object_altered_plus_minus <- refit_inla(inla.object.base, delta)
  
  descriptive_matrix_base <- extract_descriptives_inla(inla.object.base, dz, diff.logdens)
  descriptive_matrix_plus <- extract_descriptives_inla(inla_object_altered_plus_minus[[1]], dz, diff.logdens)
  descriptive_matrix_minus <- extract_descriptives_inla(inla_object_altered_plus_minus[[2]], dz, diff.logdens)
  
  # Create a descriptive matrix with 6 columns corresponding to mean.plus, sd.plus, mean.base, sd.base, mean.minus,
  # sd.minus the rows of this matrix correspond to all the parameters in the model.
  descriptive.matrix <- cbind(cbind(descriptive_matrix_minus, descriptive_matrix_base), descriptive_matrix_plus)
  colnames(descriptive.matrix) <- c("mean.minus", "sd.minus", "mean.base", "sd.base", "mean.plus", "sd.plus")
  
  return(list(descriptive.matrix = descriptive.matrix, delta = delta))
}
