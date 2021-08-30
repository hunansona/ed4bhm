H2ALL <-
function(descriptive.matrix.ls) {
  #INPUT:
  #descriptive.matrix.ls: is a list composed of the descriptive matrix and delta, where
  #descriptive matrix has 6 columns containing mean and sd for minus, base and plus model fits and
  #delta is the numerical step with w = 1-delta (w=1+delta) being the factor to weight the likelihood
  #must be an output of extract_descriptives_mbp_jags() or extract_descriptives_mbp_inla()
  #
  #OUTPUT: a matrix with columns representing ed estimates for all H2 types of 
  #5 measures c("TED", "EDL", "EDS", "pEDL", "pEDS")
  
  # descriptive.matrix <- descriptive.matrix.ls[["descriptive.matrix"]]
  # delta <- descriptive.matrix.ls[["delta"]]

  H2ALL_matrix <- matrix(NA, nrow = nrow(descriptive.matrix.ls[["descriptive.matrix"]]), ncol = 5)
  colnames(H2ALL_matrix) <- c("TED", "EDL", "EDS", "pEDL", "pEDS")
  H2ALL_matrix[, "TED"] <- ed(descriptive.matrix.ls = descriptive.matrix.ls, distance = "H2")
  H2ALL_matrix[, "EDL"] <- -ed(descriptive.matrix.ls = descriptive.matrix.ls, distance = "BCL")
  H2ALL_matrix[, "EDS"] <- -ed(descriptive.matrix.ls = descriptive.matrix.ls, distance = "BCS")
  H2ALL_matrix[, "pEDL"] <- -ed(descriptive.matrix.ls = descriptive.matrix.ls, distance = "BCL")/ed(descriptive.matrix.ls = descriptive.matrix.ls, 
                                                                                                               distance = "H2")
  H2ALL_matrix[, "pEDS"] <- -ed(descriptive.matrix.ls = descriptive.matrix.ls, distance = "BCS")/ed(descriptive.matrix.ls = descriptive.matrix.ls, 
                                                                                                               distance = "H2")
  
  return(H2ALL_matrix)
}
