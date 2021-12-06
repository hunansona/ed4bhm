mce.ed.stanfit <- function(object, B = 200, rseed = 458275, ...){
  # Same procedure as mce.ed.mcmc.list
  
  ed_base <- ed.stan( stan.object.base = object, ... )
  
  # Save the data as a matrix. If object is already a matrix -- > keep it as is 
  # If object is an S4 object -- > convert it to a matrix 
  data <- ed.stan.input( object_t = object ) # All data
  
  M <- nrow( data )
  pars <- colnames( data )
  ed_pars <- ncol( ed_base$ed )
  loop_pars <-  pars[!pars == "ll"]
  
  
  
  n_pars <- length(pars)
  num <- c(1:M)
  
  params <- array(data = NA, 
                  dim = c(B, ed_pars, length( loop_pars )), 
                  dimnames = list(c(1 : B), colnames( ed_base$ed ), loop_pars ))
  
  mcerr_pars <- matrix(data = NA, 
                       ncol = ed_pars,
                       nrow = length( loop_pars ),
                       dimnames = list(loop_pars, colnames( ed_base$ed )) )
  
  for(k in loop_pars){
    
    # we set the seed to make bootstrap computations replicable
    set.seed(rseed)
    
    for(i in 1:B)
    {
      
      
      vals_i <- sample( x = num, size = M, replace = TRUE)
      
      params[i, ,k] <- ed.stan( stan.object.base = data[vals_i, ],
                                ... )$ed[k, ]
    }
    
    mcerr_pars[k,] <- sqrt( diag( var( params[,, k] )))
  }
  
  
  tab <- cbind(ed_base$ed, mcerr_pars)
  colnames(tab) <- c("TED", "EDL", "EDS", "pEDL",
                     "pEDS", "TED_MCerr", "EDL_MCerr",
                     "EDS_MCerr", "pEDL_MCerr", "pEDS_MCerr")
  class(tab) <- c("matrix", "array", "mce.ed")
  return(tab)
}



