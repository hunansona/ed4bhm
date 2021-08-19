d2_interface_vector <-
function(avector, delta, method) {
  #INPUT
  #avector = c(mam, sdam, mb, sdb, map, sdap) (minus, base, plus) 
  #mb the mean for base posterior
  #sdb the sd for the base posterior
  #ma the mean for altered posterior
  #sda the sd for the altered posterior
  #delta for the numerical derivative computation
  #method can be H^2
  #OUTPUT
  ##the numerical second derivative for the given distance measures
  if (method == "H2") {
    D_func <- D_H2
  } else if (method == "BCmu") {
    D_func <- D_BCmu
  } else if (method == "BCsd") {
    D_func <- D_BCsd
  } else {
    cat("The method must be one of the following: H2, BCmu, BCsd.")
  }
  if (method %in% c("BCsd")) {
    return((D_func(sdb = avector[4], sda = avector[6]) - 2 * D_func(sdb = avector[4], sda = avector[4]) + D_func(sdb = avector[4], 
                                                                                                                 sda = avector[2]))/delta^2)
    
  } else {
    return((D_func(mb = avector[3], sdb = avector[4], ma = avector[5], sda = avector[6]) - 2 * D_func(mb = avector[3], 
                                                                                                      sdb = avector[4], ma = avector[3], sda = avector[4]) + D_func(mb = avector[3], sdb = avector[4], ma = avector[1], 
                                                                                                                                                                    sda = avector[2]))/delta^2)
  }
  
}
