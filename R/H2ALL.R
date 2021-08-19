H2ALL <-
function(descriptive.matrix.ls) {
  #INPUT:
  #descriptive.matrix.ls: is a list composed of the descriptive matrix and delta, where
  #descriptive matrix has 6 columns containing mean and sd for minus, base and plus model fits and
  #delta is the numerical step with w = 1-delta (w=1+delta) being the factor to weight the likelihood
  #must be an output of extract_descriptives_mbp_jags() or extract_descriptives_mbp_inla()
  #
  #OUTPUT: a matrix with columns representing ed estimates for all H2 types of 
  #5 measures c("H2total_d2", "H2mu_d2", "H2sd_d2", "qH2mu_total_d2", "qH2sd_total_d2")
  
  # descriptive.matrix <- descriptive.matrix.ls[["descriptive.matrix"]]
  # delta <- descriptive.matrix.ls[["delta"]]

  H2ALL_matrix <- matrix(NA, nrow = nrow(descriptive.matrix.ls[["descriptive.matrix"]]), ncol = 5)
  colnames(H2ALL_matrix) <- c("H2total_d2", "H2mu_d2", "H2sd_d2", "qH2mu_total_d2", "qH2sd_total_d2")
  H2ALL_matrix[, "H2total_d2"] <- ed(descriptive.matrix.ls = descriptive.matrix.ls, distance = "H2")
  H2ALL_matrix[, "H2mu_d2"] <- -ed(descriptive.matrix.ls = descriptive.matrix.ls, distance = "BCmu")
  H2ALL_matrix[, "H2sd_d2"] <- -ed(descriptive.matrix.ls = descriptive.matrix.ls, distance = "BCsd")
  H2ALL_matrix[, "qH2mu_total_d2"] <- -ed(descriptive.matrix.ls = descriptive.matrix.ls, distance = "BCmu")/ed(descriptive.matrix.ls = descriptive.matrix.ls, 
                                                                                                               distance = "H2")
  H2ALL_matrix[, "qH2sd_total_d2"] <- -ed(descriptive.matrix.ls = descriptive.matrix.ls, distance = "BCsd")/ed(descriptive.matrix.ls = descriptive.matrix.ls, 
                                                                                                               distance = "H2")
  
  return(H2ALL_matrix)
}
