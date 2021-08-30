extract_quantiles_descriptives_jags <-
function(jags.object.base, w, prec.name) {
  #INPUT:
  #jags.object.base a jags object returned by coda.samples that possibly contains deviance samples 
  #w weighting factor w = 1+-delta
  #prec.name the name(s) of the precision parameter(s) or NULL if there is no precision parameter in the model
  #OUTPUT:
  #this function takes a jags object and extracts the descriptive statistics (mean and standard deviation) of the model parameters
  #descriptive matrix with two columns (mean and sd) and rows for all the parameters of the model
  
  descriptive.matrix <- NULL
  # d.m <- NULL to find the index of the deviance sample
  deviance_index <- which(colnames(jags.object.base[[1]]) == "deviance")  #needs only one (can be the first) chain 
  # print(deviance_index)
  job <- jags.object.base  #assign the jags object to a variable before transforming it to a matrix, 
  # because in input the jags object is given as a component of a list
  pars_i_sample <- as.matrix(job)
  # print(pars_i_sample[1])
  if (!is.null(prec.name)) {
    pars_i_sample[, prec.name] <- log(pars_i_sample[, prec.name])
    # print(pars_i_sample[1])
  }
  pars_i_sample <- pars_i_sample[, -deviance_index]
  # print(pars_i_sample)
  probs_i_sample <- weight_fc_jags(jags.object.base, w)
  # print(paste(probs_i_sample, 'probs')) print( probs_i_sample)
  samples.matrix <- as.matrix(pars_i_sample, nrow = length(pars_i_sample))
  # print(paste(samples.matrix, 'samples.matrix'))
  quant0.025 <- apply(samples.matrix, 2, quantile, probs = 0.025)
  # print(paste(means, 'means'))
  quant0.5 <- apply(samples.matrix, 2, quantile, probs = 0.5)
  quant0.975 <- apply(samples.matrix, 2, quantile, probs = 0.975)
  
  descriptive.matrix <- cbind(quant0.025, quant0.5, quant0.975)
  
  row.names(descriptive.matrix) <- colnames(jags.object.base[[1]])[-deviance_index]
  
  colnames(descriptive.matrix) <- c("0.025quant", "0.5quant", "0.975quant")
  
  return(descriptive.matrix)
}
