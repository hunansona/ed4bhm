
ed.stan.matrix <- function(stan.object.base,
                           prec.name = NULL, 
                           delta = 0.01,
                           distance = "H2ALL") {
  
  # In this case stan.object.base is a matrix, so we do not need to convert it to a matrix
  
  # remove the lp__ paremeter if it is contained in the matrix
  m.names <- colnames(stan.object.base)
  lp_index <- which(m.names == "lp__") 
  if(length(lp_index)!=0) {
    stan.object.base <- stan.object.base[, -lp_index]
    cat("lp__ parameter is removed. For computations, a column called ll is needed, which contains the log-likelihood evaluated at MCMC sample of parameters")
  } 
  
  stan.object.base.modified <- stan.object.base
  
  descriptive.matrix.ls <- extract_descriptives_mbp_stan(stan.object.base.modified, 
                                                         prec.name,
                                                         delta)
  
  if (distance == "H2ALL") {
    ed.result <- H2ALL( descriptive.matrix.ls )
  }
  else {
    ed.result <- ed( descriptive.matrix.ls, distance)
  }
  rownames(ed.result) <- rownames( descriptive.matrix.ls$descriptive.matrix )
  return(list(ed = ed.result,
              descriptive.matrix.ls = descriptive.matrix.ls))
}

