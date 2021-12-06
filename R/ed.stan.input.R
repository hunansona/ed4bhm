ed.stan.input <- function(object_t){
  
  e.stan.object.base <- mcmc.list(lapply(1:ncol(object_t),
                                         function(x){mcmc(as.array(object_t)[,x,])}))
  
  new_object <- as.matrix( e.stan.object.base )
  
  # remove the lp__ paremeter if it is contained in the stan object
  m.names <- colnames(new_object)
  lp_index <- which(m.names == "lp__") 
  if(length(lp_index)!=0) {
    new_object <- new_object[, -lp_index]
    cat("lp__ parameter is removed. For computations, a column called ll is needed, which contains the log-likelihood evaluated at MCMC sample of parameters")
  } 
  
  return(new_object)
}