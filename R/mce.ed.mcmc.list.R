mce.ed.mcmc.list <- function(object, B = 200, rseed = 458275, ...){
  
  # Function compatible with ed.jags 
  # B defines the number of bootstrap draws. For not B = 200 seems to be a reasonable value
  # rseed: random seed to make bootstrap computations replicable
  # The three dots ... pass the arguments for the ed.jags function 
  
  ed_base <- ed.jags( jags.object.base = object, ... )
  
  # We combine all the chains if more than one and save the samples as matrix for 
  # easier manipulation
  data <- as.matrix(combine.mcmc( object )) # All data
  M <- nrow( data )
  pars <- colnames( data ) # Save that parameters of the model 
  ed_pars <- ncol( ed_base$ed ) # Define the number of parameters that we have ed estimates for. 
  loop_pars <-  pars[!pars == "deviance"] # Define the parameters that we need to provide mce for their ed estimates. 
  # We need to estimate the mce for all the parameters exept from the deviance 
  
  # Create objects for saving the bootstrap results
  n_pars <- length(pars)
  num <- c( 1:M )
  
  # An array with B rows, one for each bootstrap replication, length(pars) coloumns, one coloumn for each parameter
  # and one layer for each ed estimate (except from the deviance).
  # We name the rows, colnames and layers of the array with the same names of the parameters and ed.estimates names
  # In this array we are going to save the ed estimates from ed.jags for each and every bootstrap draw
  params <- array(data = NA, 
                  dim = c(B, ed_pars, length( loop_pars )), 
                  dimnames = list(c(1 : B), colnames( ed_base$ed ), loop_pars ))
  
  # Create a matrix for the standard errors of the ed estimates from each bootstrap draw
  mcerr_pars <- matrix(data = NA, 
                       ncol = ed_pars,
                       nrow = length( loop_pars ),
                       dimnames = list(loop_pars, colnames( ed_base$ed )) )
  
  
  for(k in loop_pars){
    
    # we set the seed to make bootstrap computations replicable
    set.seed(rseed)
    
    for(i in 1:B)
    {
      
      
      # We first sample the bootstrap draw from all the samples
      # Define the sample by the numbers of rows of the matrix with the data.
      vals_i <- sample( x = num, size = M, replace = TRUE)
      
      # Invoke the ed.jags function with the bootstrap draw.
      # Use only the sampled rows of the data for this step. 
      # ed.jags accepts only objects of class "mcmc.list". Thus, we create the mcmc_convert function 
      # which takes as input a "matrix" and converts it to an "mcmc.list".
      
      params[i, ,k] <- ed.jags( jags.object.base = mcmc_convert( data[vals_i, ] ),
                                ... )$ed[k, ]
      # Save the output of the ed.jags function to the array 
    }
    
    # We calculate the standard error of the bootstrap sample for each variable and we save it
    # to the matrix 
    mcerr_pars[k,] <- sqrt( diag( var( params[,, k] )))
  }
  
  # Combine the output of the ed.jags function for all the data and the estimated Monte Carlo errors
  tab <- cbind(ed_base$ed, mcerr_pars)
  colnames(tab) <- c("TED", "EDL", "EDS", "pEDL",
                     "pEDS", "TED_MCerr", "EDL_MCerr",
                     "EDS_MCerr", "pEDL_MCerr", "pEDS_MCerr")
  class(tab) <- c("matrix", "array", "mce.ed")
  return(tab)
}