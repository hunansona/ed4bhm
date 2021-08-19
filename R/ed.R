ed <-
function(descriptive.matrix.ls, distance = "H2") {
  #INPUT:
  #descriptive.matrix.ls is a list that contains descriptive matrix and delta, where the descriptive matrix must have the following columns
  #mean_plus, sd_plus, mean_base, sd_base, mean_minus, sd_minus and delta shows the numerical differentiation step 
  #distance indicates the type of the distance measure that may be one of the following H2, BCsd, BCmu
   #OUTPUT:
  #a vector of ed values for all the parameters in the model for the given distance type
  
  if (distance == "H2") {
    D_func <- D_H2
  } else if (distance == "BCsd") {
    D_func <- D_BCsd
  } else if (distance == "BCmu") {
    D_func <- D_BCmu
  } else {
    cat("The distance must be one of the following: H2, BCsd, BCmu.")
  }
  return(apply(descriptive.matrix.ls[["descriptive.matrix"]], 1, d2_interface_vector, delta = descriptive.matrix.ls[["delta"]], 
               method = distance))
}
