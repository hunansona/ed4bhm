

## Lwm1_stan
# input class: 

Lwm1_stan <- function(stan_ll, ww = 1){
  # weighting of the log-likelihood provided in ll
  return( exp(( ww - 1 ) * stan_ll ) )
  
}
