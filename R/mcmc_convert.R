
## mcmc_convert
# input class: matrix --> transform to mcmc.list (for ed.jags)

mcmc_convert <- function(object){
  
  fin_list <- mcmc.list( as.mcmc( object ) )
  
  return( fin_list )
}
