extract_median_95CrI_descriptives_stan <- function(stan.object.base.modified, 
                                                   w, prec.name= NULL) {
  m.names <- colnames(stan.object.base.modified)  
  
  # find lp__
  lp_index <- which(m.names == "lp__") 
  if(length(lp_index)==0) {
    cat("Provide lp__ either directly from Stan or evaluate the log-likelihood at sampled parameters, 
        calls this vector lp__, and pass lp__ and MCMC sample as a matrix")
  } else {
    
    
    # prec.name=NULL (no columns that need log-transformation)
    
    if (is.null(prec.name)){
      
      # extract samples that do not need any transformation
      mm <- stan.object.base.modified[, -lp_index]
      mm.names<-m.names[-lp_index]
      
      # extract lp__
      dm <- stan.object.base.modified[, lp_index] # lp__
      
      # collect samples for further computations
      samples.matrix <- as.matrix(mm)
      #colnames(samples.matrix) <- mm.names
      
      # initiate the matrix to collect descriptive (mean and sd) statistics
      descriptive.matrix <- matrix(data = NA,
                                   nrow = length( mm.names ), ncol = 3,
                                   dimnames = list(mm.names, c("0.025quant", "0.5quant", "0.975quant")) )
      
      # wigthting of the likelihood given lp__
      Lwm1_value <- Lwm1_stan(stan_lp = dm, ww = w) 
      #  normalizing constant
      cte_stan <- sum( Lwm1_value / length( Lwm1_value ) )
      # new probability assigned to each observation which is used for the computation of descritive statistics
      pp  <- (Lwm1_value / length( Lwm1_value )) / cte_stan
      
      for (i in 1: length(mm.names)){
        mm_sim_i <- samples.matrix[, i]
        descriptive.matrix[i, 1]<-  quantile(mm_sim_i, probs = 0.025)
        #sum( mm_sim_i * pp ) # mean of the perturbed distribution
        descriptive.matrix[i, 2]<-quantile(mm_sim_i, probs = 0.5)
        descriptive.matrix[i, 3]<-quantile(mm_sim_i, probs = 0.975)
        #sqrt( sum(mm_sim_i^2 * pp ) - (sum(mm_sim_i * pp))^2) # sd of the perturbed distribution
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
      mm <- stan.object.base.modified[, -c(lp_index, prec_index)]
      mm.names<-m.names[-c(lp_index, prec_index)]
      
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
      
      # extract lp__
      dm <- stan.object.base.modified[, lp_index] # lp__
      
      # collect samples for further computations
      samples.matrix <- as.matrix( cbind(mm, lm) )
      colnames(samples.matrix) <-c( mm.names, lm.names )
      
      # initiate the matrix to collect descriptive (mean and sd) statistics
      descriptive.matrix <- matrix(NA, nrow = length(c(mm.names, lm.names)), ncol = 3)
      rownames(descriptive.matrix)<-c(mm.names, lm.names)
      colnames(descriptive.matrix)<-c("0.025quant", "0.5quant", "0.975quant")
      
      # weigthting of the likelihood given lp__
      Lwm1_value <- Lwm1_stan(stan_lp = dm, ww = w) 
      #  normalizing constant
      cte_stan <- sum(Lwm1_value / length( Lwm1_value ))
      # new probability assigned to each observation which is used for the computation of descritive statistics
      pp  <- (Lwm1_value / length( Lwm1_value )) / cte_stan
      
      for (i in 1: length(c(mm.names,lm.names))){
        mm_sim_i <- samples.matrix[, i]
        # descriptive.matrix[i, 1]<- sum( mm_sim_i * pp ) # mean of the perturbed distribution
        # descriptive.matrix[i, 2]<-sqrt(sum(mm_sim_i^2 * pp) - (sum( mm_sim_i * pp))^2) # sd of the perturbed distribution
        descriptive.matrix[i, 1]<-  quantile( mm_sim_i, probs = 0.025)
        #sum( mm_sim_i * pp ) # mean of the perturbed distribution
        descriptive.matrix[i, 2]<-quantile( mm_sim_i, probs = 0.5)
        descriptive.matrix[i, 3]<-quantile( mm_sim_i, probs = 0.975)
      }
    }
    return(descriptive.matrix) 
  }
}