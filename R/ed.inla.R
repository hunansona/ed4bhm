ed.inla <-
function(inla.object.base,  distance = "H2", dz, diff.logdens,
                                    delta = 0.01) {
  #INPUT:
  #this function takes the inla fit object from the base model as input and returns the ed values as requested
  ##delta is the numerical step and 1-delta (1+delta) is the factor you want to weight the likelihood
  ##log.scale: TRUE if you need the log scale for the hyperparameter (internal scale for INLA)
  #distance: indicates the type of the distance measure that may be either 
  #Hellinger^2 or  H2ALL (contains 5 measures based on H^2, see detail in H2ALL function)
  #delta: shows the numerical differentiation step 
  #OUTPUT:
  #a list composed of the descriptive matrix.ls(which is a list of descriptive matrix and delta) and
  #a vector (for one measure only) or 
  #a matrix (H2ALL measures) of ed values
  
  #descriptive.matrix.ls contains the descriptive matrix and the delta value
  descriptive.matrix.ls <- extract_descriptives_mbp_inla(inla.object.base, delta, dz, diff.logdens)
  
  
  if (distance == "H2ALL") {
    ed.result <- H2ALL(descriptive.matrix.ls)
  } else {
    ed.result <- ed(descriptive.matrix.ls, distance)
  }
  rownames(ed.result) <- rownames(descriptive.matrix.ls$descriptive.matrix)
  
  return(list(ed = ed.result, descriptive.matrix.ls = descriptive.matrix.ls))
}
