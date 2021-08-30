ed <-
function(descriptive.matrix.ls, distance = "H2ALL") {
  #INPUT:
  #descriptive.matrix.ls is a list that contains descriptive matrix and delta, where the descriptive matrix must have the following columns
  #mean_plus, sd_plus, mean_base, sd_base, mean_minus, sd_minus and delta shows the numerical differentiation step 
  #distance indicates the type of the distance measure that may be one of the following H2, BCS, BCL
   #OUTPUT:
  #a vector of ed values for all the parameters in the model for the given distance type
  
  if (distance == "H2") {
    D_func <- D_H2
  } else if (distance == "BCS") {
    D_func <- D_BCS
  } else if (distance == "BCL") {
    D_func <- D_BCL
  } else {
    cat("The distance must be one of the following: H2, BCS, BCL.")
  }
  return(apply(descriptive.matrix.ls[["descriptive.matrix"]], 1, d2_central_difference, delta = descriptive.matrix.ls[["delta"]], 
               method = distance))
}
