expectation_fc <-
function(ISweight, samples) {
  #INPUT:
  ## ISweight are the importance sampling weights
  ## samples the MCMC samples to calculate the expectation for
  #OUTPUT:
  #computes the expectation of an MCMC weighted sample via impotance sampling
  #expectation of an MCMC weighted sample according to Held's book (Applied statistical inference) page 266, equation (8.15)
 
  expectw <- (1/sum(ISweight)) * sum(ISweight * samples)
  
  return(expectw)
}
