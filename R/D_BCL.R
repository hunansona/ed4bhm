D_BCL <-
function(mb, sdb, mw, sdw) {
  #INPUT
  ##mb the mean for base posterior
  ##sdb the sd for the base posterior
  ##mw the mean for altered posterior
  ##sdw the sd for the altered posterior
  #OUTPUT
  ##the mean change part of the BC between 2 normal densities, equation (10) in si4bayesmeta manuscript
  # options(digits = 22)
  DBCL <- exp(-((mb - mw)^2)/(4 * (sdb^2 + sdw^2)))
  return(DBCL)
  # options(digits = 7)
}
