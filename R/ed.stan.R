ed.stan <- function(stan.object.base,
                    prec.name = NULL, 
                    delta = 0.01,
                    distance = "H2ALL") UseMethod(generic = "ed.stan")
#H2ALL computation for stan
#this function takes the stan fit object from the base model as input and returns the identification values, descritive.matrix and delta
#INPUT:
##stan.object.base: stan fit object from the base model with a column called ll, which contains the log-likelihood evaluated at MCMC sample of parameters
##delta: is the numerical step and 1-delta (1+delta) is the factor you want to weight the likelihood
##prec.name: names of parameters (in JAGS and INLA precisions but in Stan standard deviations) to be log-transformed
#OUTPUT:
#a list composed of
#a matrix (H2ALL measures) of identification values
#the descriptive matrix.ls(which is a list of descriptive matrix and delta)

