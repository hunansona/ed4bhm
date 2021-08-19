sd_fc <-
function(ISweight, samples) {
  #INPUT:
  ## ISweight are the importance sampling weights
  ## samples the MCMC samples to calculate the expectation for
  #OUTPUT:
  #computes the sd of an MCMC weighted sample via impotance sampling
  #sd of an MCMC weighted sample 
  #expectation_posterior_altered <- sum(ISweight * samples)
  sdw <- sqrt(expectation_fc(ISweight, samples^2) - (expectation_fc(ISweight, samples))^2)
  
  return(sdw)
}
