D_BCsd <-
function(sdb, sda) {
  #INPUT
  ##sdb the sd for the base posterior
  ##sda the sd for the altered posterior
  #OUTPUT
  ##the sd part (scaling) of the BC between 2 normal densities, equation (10) in si4bayesmeta manuscript
  DBCsd <- sqrt((2 * sdb * sda)/(sdb^2 + sda^2))
  
  return(DBCsd)
}
