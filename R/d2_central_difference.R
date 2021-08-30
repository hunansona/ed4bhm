d2_central_difference <-
function(desc_vector, method , delta = 0.01) {
  #INPUT
  #desc_vector = c(mwm, sdwm, mb, sdb, mwp, sdwp) (minus, base, plus) 
  #mb the mean for base posterior
  #sdb the sd for the base posterior
  #mw the mean for altered posterior
  #sdw the sd for the altered posterior
  #delta for the numerical derivative computation
  #method can be H^2, BCL, BCS
  #OUTPUT
  ##the numerical second derivative for the given distance measures
  if (method == "H2") {
    D_func <- D_H2
  } else if (method == "BCL") {
    D_func <- D_BCL
  } else if (method == "BCS") {
    D_func <- D_BCS
  } else {
    cat("The method must be one of the following: H2, BCL, BCS.")
  }
  if (method %in% c("BCS")) {
    return((D_func(sdb = desc_vector[4], sdw = desc_vector[6]) - 2 * D_func(sdb = desc_vector[4], sdw = desc_vector[4]) + D_func(sdb = desc_vector[4], 
                                                                                                                 sdw = desc_vector[2]))/delta^2)
    
  } else {
    return((D_func(mb = desc_vector[3], sdb = desc_vector[4], mw = desc_vector[5], sdw = desc_vector[6]) - 2 * D_func(mb = desc_vector[3], 
                                                                                                      sdb = desc_vector[4], mw = desc_vector[3], sdw = desc_vector[4]) + D_func(mb = desc_vector[3], sdb = desc_vector[4], mw = desc_vector[1], 
                                                                                                                                                                    sdw = desc_vector[2]))/delta^2)
  }
  
}
