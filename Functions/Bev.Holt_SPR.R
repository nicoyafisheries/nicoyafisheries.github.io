#Beverton-holt Spawner Recruit relationship

#Requires:

# R0 <- pristine recruits

# M <- natural mortality

# fa <- fecundity (weight*egg per gram)

# Ma <- vector of maturity probabilities

Bev.Holt_SPR <- function(R0, M, fa, Ma){
  
  R <- seq(from = 0, to = R0, by = (R0/10))
  
  
  Egg.Output <- vector(length = length(R))
  
  for (i in 1:length(R)){
  
  N_at[1,1] = R[i]
  
  
    #  Set inital condition in year t = 0 (assumes no fishing)
    for (a in 1:17){
      
      N_at[1,a+1] = N_at[1,(a)]*exp(-M)
      
    }
  
  Egg.Output[i] <- sum(N_at[1,]*fa*Ma)
  
  }
  
  Egg.Output <<- Egg.Output
  
  return(plot((Egg.Output/Egg.Output[11]), (R/R0), ylab = "Recruitment/R0", xlab = "Egg Output/E0"))
  
}





# SPR0 <- sum(N_at[1,]*fa*Ma)
# 
# S0 <- SPR0*R0
# 
# BHa <- S0*(1-h)/(4*h*R0)
#   
# BHb <- (5*h-1)/(4*h*R0) 
