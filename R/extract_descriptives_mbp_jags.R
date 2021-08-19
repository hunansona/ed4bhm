extract_descriptives_mbp_jags <-
function(jags.object.base, delta, prec.name) {
  #INPUT:
  #jags.object.base a jags object from the base model fit returned by coda.samples that possibly contains deviance samples 
  #delta is the numerical differentiation step: w = 1+-delta
  #prec.name the name of the precision parameter or NULL if there is no precision parameter in the model
  #OUTPUT: 
  #a list of the descriptive matrix (means and sds for minus, base, plus models) and the delta 
  descriptive_matrix_minus <- extract_descriptives_jags(jags.object.base, w = 1 - delta, prec.name)
  descriptive_matrix_base <- extract_descriptives_jags(jags.object.base, w = 1, prec.name)
  descriptive_matrix_plus <- extract_descriptives_jags(jags.object.base, w = 1 + delta, prec.name)
  
  
  # Create a descriptive matrix with 6 columns corresponding to mean.minus, sd.minus, mean.base, sd.base, mean.plus,
  # sd.plus the rows of this matrix correspond to all the parameters in the model.
  descriptive.matrix <- cbind(cbind(descriptive_matrix_minus, descriptive_matrix_base), descriptive_matrix_plus)
  colnames(descriptive.matrix) <- c("mean.minus", "sd.minus", "mean.base", "sd.base", "mean.plus", "sd.plus")
  
  return(list(descriptive.matrix = descriptive.matrix, delta = delta))
}
