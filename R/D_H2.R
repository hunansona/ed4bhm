D_H2 <-
function(mb, sdb, mw, sdw) {
  #INPUT
  ##mb the mean for base posterior
  ##sdb the sd for the base posterior
  ##mw the mean for altered posterior
  ##sdw the sd for the altered posterior
  #OUTPUT
  ##the squared Hellinger distance between two normal densities parametrized by mean and sd
  Dh2 <- 1 - sqrt(2 * sdb * sdw/(sdb^2 + sdw^2)) * exp(-(mb - mw)^2/(4 * (sdb^2 + sdw^2)))
  
  return(Dh2)
}
