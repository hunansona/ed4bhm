D_BCmu <-
function(mb, sdb, ma, sda) {
  #INPUT
  ##mb the mean for base posterior
  ##sdb the sd for the base posterior
  ##ma the mean for altered posterior
  ##sda the sd for the altered posterior
  #OUTPUT
  ##the mean change part of the BC between 2 normal densities, equation (10) in si4bayesmeta manuscript
  # options(digits = 22)
  DBCmu <- exp(-((mb - ma)^2)/(4 * (sdb^2 + sda^2)))
  return(DBCmu)
  # options(digits = 7)
}
