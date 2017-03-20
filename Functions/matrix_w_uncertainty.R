
matrix_w_uncertainty = function(Lc, Z, Loo, k, t0, maxAge,time_horizon = 100, Wa = 0.024, Wb = 2.824, m50 = 55, m95 = 62.7, females = 0.6, h = 0.7, SP_unfished = 287860317){

  #just to start while loop
  Linf = -1
  
  K = -1
  
  while( Linf < 0){
  Linf = rnorm(1, Loo, sqrt(164.6089))
  
  }
  
  while( K < 0 ) {
  K = rnorm(1, k, sqrt(0.002916))
  
  }
  
  to = rnorm(1,t0, sqrt(0.4096))
  
  M = K *1.5
  
  #Our max age is 18 years
  Age <- seq(1,maxAge,1)
  
  #Length at median age 
  Length <- Linf * (1 - exp(-K * ((Age + 0.5) - to)))
  
  #Converts lengths (cm) to weight (g)
  Weight <- Wa*Length^Wb
  
  # Probabilty of Maturity
  Ma = 1/(1 + exp(-log(19)*((Length - m50)/(m95 - m50))))
  
  #realitve fecundity - produce about 41 eggs per gram 
  #SOURCE: https://www.scribd.com/doc/81002346/Crecimiento-y-Reproduccion-de-corvina
  fa <- Weight*41  
  
  
  
  
Lc = Lc
Va = ifelse(Length < Lc, 0, 1)

#fishing mortality

F = Z - M 

#survival probabilty

Sa <- exp(-M - ( Va * (F)))


#r build_matrix

A = matrix(data = 0, nrow = maxAge, ncol = maxAge)


for (r in 2:18) {
  
  j = r - 1 
  
  A[r, j] =  Sa[j]
  
}

A[maxAge, maxAge] = Sa[maxAge]

#run sim

N0 = as.numeric(IC[1, ])

pop <- matrix(0, maxAge, time_horizon + 1)
pop[, 1] <- N0

for (i in 1:time_horizon) {
  # Survival
  pop[, i + 1] <- A %*% pop[, i]
  # Reproduction
  Ne <- sum(fa * Ma * females * pop[, i])
  
  
  pop[1, i + 1] <- (560 * Ne) / (17271619 + ((h - 0.2) * Ne))

  
}

SPR <- apply(pop, 2, FUN = function(x) (sum(fa * Ma * females * x) / SP_unfished))

}