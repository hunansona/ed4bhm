
## extract_descriptives_stan
# input class: 

extract_descriptives_stan <- function(stan.object.base.modified, w, prec.name= NULL) {
  #This function takes a stan object and extracts the descriptive statistics (mean and standard deviation) of the model parameters
  #INPUT:
  #stan.object.base.modified: a stan object in matrix form with a column called ll, which contains the log-likelihood evaluated at MCMC sample of parameters 
  #w: weighting factor w = 1+-delta
  #prec.name: names of parameters (in JAGS and INLA precisions but in Stan standard deviations) for transformation to the log-scale
  #OUTPUT:
  #descriptive matrix with two columns (mean and sd) and rows for all the parameters of the model
  
  # organize the stan.object.base.modified
  
  # # create a matrix
  # #e.stan.object.base <- extract(stan.object.base)# does not work correctly
  # e.stan.object.base<-mcmc.list(lapply(1:ncol(stan.object.base),function(x){mcmc(as.array(stan.object.base)[,x,])}))
  # m.stan.object.base <- as.matrix(e.stan.object.base)
  
  # collect all colnames
  m.names <- colnames(stan.object.base.modified)  
  
  # find ll
  ll_index <- which(m.names == "ll") 
  if(length(ll_index)==0) {
    cat("Provide a ll column, which contains the log-likelihood evaluated at MCMC sample of parameters, either directly from Stan (generated quantities) or evaluate the log-likelihood at sampled parameters, 
        calls this vector ll, and pass ll and MCMC sample as a matrix")
  } else {
    
    
    # prec.name=NULL (no columns that need log-transformation)
    
    if (is.null(prec.name)){
      
      # extract samples that do not need any transformation
      mm <- stan.object.base.modified[, -ll_index]
      mm.names<-m.names[-ll_index]
      # extract the log-likelihood contained in ll
      dm <- stan.object.base.modified[, ll_index] # ll
      
      # collect samples for further computations
      samples.matrix <- as.matrix(mm)
      #colnames(samples.matrix) <- mm.names
      
      # initiate the matrix to collect descriptive (mean and sd) statistics
      descriptive.matrix <- matrix(data = NA,
                                   nrow = length( mm.names ), ncol = 2,
                                   dimnames = list(mm.names, c("mean", "sd")) )
      
      # weigthting of the likelihood given ll
      Lwm1_value <- Lwm1_stan(stan_ll = dm, ww = w) 
      print(Lwm1_value)
      #  normalizing constant
      cte_stan <- sum( Lwm1_value / length( Lwm1_value ) )
      # new probability assigned to each observation which is used for the computation of descritive statistics
      pp  <- (Lwm1_value / length( Lwm1_value )) / cte_stan
      
      for (i in 1: length(mm.names)){
        mm_sim_i <- samples.matrix[, i]
        descriptive.matrix[i, 1]<- sum( mm_sim_i * pp ) # mean of the perturbed distribution
        descriptive.matrix[i, 2]<-sqrt( sum(mm_sim_i^2 * pp ) - (sum(mm_sim_i * pp))^2) # sd of the perturbed distribution
      }
    } else {
      
      # prec.name!=NULL (there are columns that need log-transformation)
      
      # find samples that must be log-transformed 
      if(length(prec.name)>0) {
        prec_index <- rep(NA, length( prec.name ))
        for (i in 1:length( prec.name )){
          prec_index[i] <- which( m.names == prec.name[i] )
        }
      }
      
      # extract samples that do not need any transformation
      mm <- stan.object.base.modified[, -c(ll_index, prec_index)]
      mm.names<-m.names[-c(ll_index, prec_index)]
      
      # extract and log-transform samples that mast be log-transformed
      if(length( prec_index ) > 0){
        lm <- log(stan.object.base.modified[, prec_index]) # log-transformed samples
      }
      if(length( prec_index ) > 0){
        # although computations are conducted on the log-scale, we do not change the name of the parameter
        # lm.names<-paste("log_", m.names[prec_index], sep="") 
        lm.names <- m.names[prec_index]
      } else {
        lm.names <- NULL
      }
      
      # extract the log-likelihood contained in ll
      dm <- stan.object.base.modified[, ll_index] # ll
      
      # collect samples for further computations
      samples.matrix <- as.matrix( cbind(mm, lm) )
      colnames(samples.matrix) <-c( mm.names, lm.names )
      
      # initiate the matrix to collect descriptive (mean and sd) statistics
      descriptive.matrix <- matrix(NA, nrow = length(c(mm.names, lm.names)), ncol = 2)
      rownames(descriptive.matrix)<-c(mm.names, lm.names)
      colnames(descriptive.matrix)<-c("mean", "sd")
      
      # weigthting of the likelihood given ll
      Lwm1_value <- Lwm1_stan(stan_ll = dm, ww = w) 
      #  normalizing constant
      cte_stan <- sum(Lwm1_value / length( Lwm1_value ))
      # new probability assigned to each observation which is used for the computation of descritive statistics
      pp  <- (Lwm1_value / length( Lwm1_value )) / cte_stan
      
      for (i in 1: length(c(mm.names,lm.names))){
        mm_sim_i <- samples.matrix[, i]
        descriptive.matrix[i, 1]<- sum( mm_sim_i * pp ) # mean of the perturbed distribution
        descriptive.matrix[i, 2]<-sqrt(sum(mm_sim_i^2 * pp) - (sum( mm_sim_i * pp))^2) # sd of the perturbed distribution
      }
    }
    return(descriptive.matrix) 
  }
}

