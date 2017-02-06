###################################
#Deterministic Age-Stuctured Model
###################################
#####Parameters
# Linf <- Von Bertalanffy L infinity
# K <- Von Bertalanffy growth parameter
# to <- Von Bertalanffy theoretical length of fish age 0
# BHa <- Beverton Holt alpha
# BHb <- Beverton Holt beta
# Va <- vulnerability to fishing at Age = a
# Ma <- Maturity at Age = a
# fa <- fecundity at Age = a
# M <- nautal mortality
# F <- fishing mortality
######OUTPUT
# N_at <- dataframe of number of individuals for every age a (cols) at time t (rows)

rm(list=ls())


#Set Model----
NumYears = 50

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

F <- 0.3

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

Recruitment <- vector(length = NumYears)

Catch <- data.frame(matrix(data = NA, ncol = max(Age) + 1, nrow = NumYears))
colnames(Catch) <- as.character(c(Age,'Total'))
                        

Revenue <- data.frame(matrix(data = NA, ncol = 4, nrow = NumYears))
colnames(Revenue) =c('small',
                     'med',
                     'large',
                     'total')


#Generate Population Data--------
N_at[1,1] = R0

for (t in 2:NumYears){
  
  for (a in 1:17){
  
  #  Set inital condition in year t = 0 (assumes no fishing)
    N_at[1,a+1] = N_at[1,(a)]*exp(-M)
    
    Recruitment[1] = sum(N_at[1,]*fa*Ma)
    
    Catch[1,] = 0
    
  # Introduce fishing as Va*F  
    N_at[t,a+1] = exp(-M - Va[a]*F)*N_at[(t - 1),(a)]
    
    N_at[t,1] = Recruitment[t - 1]/(BHa + BHb*Recruitment[t - 1])
    
    Recruitment[t] = sum(N_at[t,]*fa*Ma)
    
    #Catch by cohort = number of individual age a in year t - indvid. age a captured in year t
    
     Catch[t,a+1] = N_at[(t),(a+1)] - exp( - Va[a+1]*F)*N_at[(t),(a+1)]   
    
     Catch[t,1] = 0
    
  }
}




# Calculate Revenue by commercial class------

for (t in 1:NumYears){

  Revenue[t,1] = sum(Weight[1:2]*Catch[t,1:2]*Price[1])
  
  Revenue[t,2] = sum(Weight[3:5]*Catch[t,3:5]*Price[2])
  
  Revenue[t,3] = sum(Weight[5:max(Age)]*Catch[t,5:max(Age)]*Price[3])
  
  Revenue[t,4] = sum(Revenue[t,1:3])
  
}

# 
# # Calculate revenue in year t
# if (a < 2) {
#   Revenue[t,a] = Weight[a]*N_at[t,a]*Price[1]
#   
# } else if (a > 2 & a < 6) {
#   Revenue[t,a] = Weight[a]*N_at[t,a]*Price[2]
#   
# } else {
#   Revenue[t,a] = Weight[a]*N_at[t,a]*Price[3]
# }
# 
# #Revenue[t,max(Age)] = Weight[max(Age)]*N_at[t,max(Age)]*Price[3]
# 



# for (t in 1:NumYears){
#   
#   Catch[t,1] = sum(Weight[1:2]*N_at[t,1:2]*Price[1])
#   
#   Catch[t,2] = sum(Weight[3:5]*N_at[t,3:5]*Price[2])
#   
#   Catch[t,3] = sum(Weight[5:max(Age)]*N_at[t,5:max(Age)]*Price[3])
#   
#   Catch[t,4] = sum(Revenue[t,1:3])
#   
# }
# 