ed.jags <-
function(jags.object.base, delta = 0.01, prec.name,
                                    distance = "H2") {
  #INPUT:
  ##jags.object.base: jags fit object from the base model
  ##delta: is the numerical step and 1-delta (1+delta) is the factor you want to weight the likelihood
  #prec.name the name of the precision parameter or NULL if there is no precision parameter in the model
  #distance: indicates the type of the distance measure that may be either 
  #Hellinger^2 or H2ALL (contains 5 measures based on H^2, see detail in H2ALL function)
  #derivative.order: allows the user to choose the order of the derivative representing the ed measure, 
  ##it may be "first" or "second", not needed for the option distance = "H2ALL"
  #derivative.type: is needed if the derivative.order is chosen as "first" and represents the 
  ##direction of the first derivative, it can be "plus" or "minus", not needed for the option distance = "H2ALL"
  #OUTPUT:
  #this function takes the jags fit object from the base model as input and returns the ed values as requested
  #a list composed of a vector (for one measure only) or 
  #a matrix (H2ALL measures) of ed values
  #the descriptive matrix.ls(which is a list of descriptive matrix and delta) and

  
  #descriptive.matrix.ls contains the descriptive matrix and the delta value
  descriptive.matrix.ls <- extract_descriptives_mbp_jags(jags.object.base, delta, prec.name)
  
  
  if (distance == "H2ALL") {
    ed.result <- H2ALL(descriptive.matrix.ls)
  } else {
    ed.result <- ed(descriptive.matrix.ls, distance)
  }
  rownames(ed.result) <- rownames(descriptive.matrix.ls$descriptive.matrix)
  
  return(list(ed = ed.result, descriptive.matrix.ls = descriptive.matrix.ls))
}
