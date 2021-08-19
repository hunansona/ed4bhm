refit_inla <-
function(inla.object.base, delta) {
  #INPUT:
  #this function takes the inla fit object from the base model as input and fits weighted inla models with plus and minus
  #delta is the numerical delta and 1-delta (1+delta) is the factor you want to weight the likelihood
  #OUTPUT:
  #differs from refit_inla by calculation of ndat dynamically
  #list of inla objects altered.plus and altered.minus
  
  numdat <- ndat(inla.object.base)  #number of data points in the input data
  
  result_m <- inla.object.base
  result_p <- inla.object.base
  
  ### weighting in INLA
  result_m$.args$weights <- rep(1 - delta, numdat)  #for minus
  result_p$.args$weights <- rep(1 + delta, numdat)  #for plus
  
  # computation of weighted results
  inla.setOption(enable.inla.argument.weights = TRUE)
  inla.object.altered.p <- inla.rerun(result_p, plain = TRUE)
  inla.object.altered.m <- inla.rerun(result_m, plain = TRUE)
  
  return(list(inla.object.altered.plus = inla.object.altered.p, inla.object.altered.minus = inla.object.altered.m))
}
