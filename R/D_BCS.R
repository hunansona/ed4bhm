D_BCS <-
function(sdb, sdw) {
  #INPUT
  ##sdb the sd for the base posterior
  ##sdw the sd for the altered posterior
  #OUTPUT
  ##the sd part (scaling) of the BC between 2 normal densities
  DBCS <- sqrt((2 * sdb * sdw)/(sdb^2 + sdw^2))
  
  return(DBCS)
}
