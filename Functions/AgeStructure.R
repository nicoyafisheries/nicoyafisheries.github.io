###################################
#Deterministic Age-Stuctured Model
###################################
#####Parameters
# BHa <- Beverton Holt alpha
# BHb <- Beverton Holt beta
# Va <- vulnerability to fishing at Age = a
# Ma <- Maturity at Age = a
# fa <- fecundity at Age = a
######OUTPUT
# N_at <- dataframe of number of individuals for every age a (cols) at time t (rows)

rm(list=ls())


#Set Model----
NumYears = 10

R0 = 10000


#Parameters----
Linf = 122.1
K = 0.172
to = 0.919

Wa = 0.3
  
Wb = 2.824


S50 = 26

S95 = 29


m50 = 55

m95 = 62.7


M <- 0.3

F <- 0.258

BHa <- 0.546 #this is made up
  
BHb <- 0.00089 #this is made up
  
  
#Data-----
Age <- seq(1,18,1)
Length <- Linf*(1-exp(-K*(Age-to)))
Weight <- Wa*Length^Wb


Va = 1/(1 + exp(-log(19)*((Length - S50)/(S95 - S50))))

Ma =  1/(1 + exp(-log(19)*((Length - m50)/(m95 - m50))))

fa <- Weight*.001

Price <- c(100,500,1000)

#Data Structures----  
N_at <- data.frame(matrix(data = NA, ncol = max(Age), nrow = NumYears))
colnames(N_at) = as.character(c(Age))

delta_Nt <- data.frame(matrix(data = NA, ncol = 2, nrow = NumYears))
colnames(delta_Nt) <- c('Recruitment',
                        'Catch')

Revenue <- data.frame(matrix(data = NA, ncol = max(Age+1), nrow = NumYears))
colnames(Revenue) = as.character(c(Age,'Total'))

N_at[1,1] = R0

for (t in 2:NumYears){
  
  for (a in 1:17){
  
  #  Set inital condition in year t = 0 (assumes no fishing)
    N_at[1,a+1] = N_at[1,(a)]*(1-M)
    
    delta_Nt$Recruitment[1] = sum(N_at[1,]*fa)
    
  # Introduce fishing as Va*F  
    N_at[t,a+1] = exp(-M - Va[a]*F)*N_at[(t-1),(a)]
    
    N_at[t,1] = delta_Nt$Recruitment[t-1]/(BHa + BHb*delta_Nt$Recruitment[t-1])
    
    delta_Nt$Recruitment[t] = sum(N_at[t,]*fa*Ma)
    
  # Calculate revenue in year t
    if (a-1 < 2){
      Revenue[t,a] = Weight[a]*N_at[t,a]*Price[1]
      
    } else if (a > 2 & a < 6){
      Revenue[t,a] = Weight[a]*N_at[t,a]*Price[2]
      
    } else {
      Revenue[t,a] = Weight[a]*N_at[t,a]*Price[3]
    }
    
Revenue$Total[t] = sum(Revenue[t,1:max(Age)])    
  }
}

