
## ed.stan.stanfit
# input class: stanfit
ed.stan.stanfit <- function(stan.object.base,
                            prec.name = NULL, 
                            delta = 0.01,
                            distance = "H2ALL") {
  
  # convert the information to a matrix for better manipulation. 
  stan.object.base.modified <- ed.stan.input(stan.object.base)
  
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

