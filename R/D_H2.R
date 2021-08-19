D_H2 <-
function(mb, sdb, ma, sda) {
  #INPUT
  ##mb the mean for base posterior
  ##sdb the sd for the base posterior
  ##ma the mean for altered posterior
  ##sda the sd for the altered posterior
  #OUTPUT
  ##the squared Hellinger distance between two normal densities parametrized by mean and sd
  Dh2 <- 1 - sqrt(2 * sdb * sda/(sdb^2 + sda^2)) * exp(-(mb - ma)^2/(4 * (sdb^2 + sda^2)))
  
  return(Dh2)
}
