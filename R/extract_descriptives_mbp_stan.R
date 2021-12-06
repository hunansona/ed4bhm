
#### Required for ed.stan ####

## extract_descriptives_mbp_stan
# Input class: 


extract_descriptives_mbp_stan <- function(stan.object.base.modified, prec.name, delta) {
  #This function applies weighting to the stan.object.base.modified
  #INPUT:
  #stan.object.base.modified: a stan object from the base model in matrix form with a column called ll, which contains the log-likelihood evaluated at MCMC sample of parameters 
  #prec.name: names of parameters (in JAGS and INLA precisions but in Stan standard deviations) for the log-transformation
  #delta: the numerical differentiation step: w = 1+/-delta
  #OUTPUT: 
  #a list of the descriptive matrix (means and sds for minus, base, plus models) and the delta 
  
  descriptive_matrix_base <- extract_descriptives_stan(stan.object.base.modified, w = 1, prec.name)
  descriptive_matrix_plus <- extract_descriptives_stan(stan.object.base.modified, w = 1+delta, prec.name)
  descriptive_matrix_minus <- extract_descriptives_stan(stan.object.base.modified, w = 1-delta, prec.name)
  
  #Create a descriptive matrix with 6 columns corresponding to mean.minus, sd.minus, mean.base, sd.base, mean.plus, sd.plus
  #the rows of this matrix correspond to all the parameters in the model.
  descriptive.matrix <- cbind(descriptive_matrix_minus, descriptive_matrix_base, descriptive_matrix_plus)
  colnames(descriptive.matrix) <- c("mean.minus", "sd.minus", "mean.base", "sd.base", "mean.plus", "sd.plus")
  
  return(list("descriptive.matrix" = descriptive.matrix, "delta" = delta))
}

