weight_fc_jags <-
function(jags.obj, w) {
  #INPUT:
  #   #jags.obj a jags object from the jags fit of the base model
  #   #w = 1+-delta
  #OUTPUT:
  #computes the probability weights by fast computation
  #a vector of probability weights
  
  if (is.null(jags.obj[, "deviance"])) {
    cat("run the jags model again to get also the deviance sample. To get the deviance sample in jags, load the module dic and add the deviance in the jags parameter list")
  } else {
    deviance_sample <- unlist(jags.obj[, "deviance"])  #deviance samples 
  }
  # samples_propto_lik <- exp((deviance_sample)/(-2)) #likelihood samples = pi(\bm{y} \mid m, B) probs_i_sample <-
  # (samples_propto_lik)^(w - 1) #weighted likelihood samples = (\pi(\bm{y} \mid m, B))^(w-1)
  
  probs_i_sample <- exp(-((w - 1) * deviance_sample)/2)  #weighted likelihood samples = (\pi(\bm{y} \mid m, B))^(w-1)
  
  return(probs_i_sample)
}
