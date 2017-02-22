
Model_fun = function(Z,Lc,months.open) {
  # Z = x[[1]]
  # Z = x$Z
  # 
  # Lc = x$Lc #[[2]]
  # 
  # months.open = x$months.open #x[[3]]

  R0. = R0
  maxAge. = maxAge
  h. = h
  fa. = fa
  Ma. = Ma
  Length. = Length
  Weight. = Weight
  M. = M
  
  
  Mseasonal = M. / 12
  
  #F converted to monthly
  
  F = (Z - M) / 12
  
  F2012 = Z.2012 - M
  

  
  # track model progress
  # print(paste("F", F, sep = " = "))
  # 
  # print(paste("Lc", x$Lc, sep = " = "))
  # 
  # print(x$months.open[1])
  
  #Pristine Inital condition----    
  
  # R0 <- unfished recruits
  unfished[1] = R0.
  # 
  # 
  # estimate age structure of unfished pop
  for (a in 2:(maxAge.)) {
    unfished[a] = unfished[ (a - 1)] * exp(-M.)
    
  }
  
  
  #Overfished Inital condition-----
  N_at[1,] = InitialCondition
  
  Biomass[1] = sum(N_at[1, 1:maxAge.] * Weight.)
  
  #Length (cm) at which a fish is vulnerable to fishing mortality----
  Va = ifelse(Length < Lc, 0, 1)
  
  #Eggs produced in year 1
  Eggs[1] = sum(N_at[1, ] * fa. * Ma. * females)
  
  phi <- sum(unfished * fa. * Ma. * females)
  
  
  Catch[1,] = (((Va * F2012) / (Va * F2012 + M)) * N_at[1, ] * (1 - exp(-M - Va * F2012)))*(Weight/1000)
  
  CatchTotal[1] = sum(Catch[1, 1:maxAge])
  
  # no revenue in year 1
  Revenue[1, 1] = sum(Catch[1, 1:2] * Price[1] )
  
  Revenue[1, 2] = sum( Catch[1, 3:4] * Price[2] )
  
  Revenue[1, 3] = sum(Catch[1, 5:maxAge.] * Price[3] )
  
  RevenueTotal[1] = sum(Revenue[1, 1:3])
  
  #Estimate marginal cost
  
  c = RevenueTotal[1] / F2012
  
  
  open = c(rep(0, (12 - months.open)), rep(1, (months.open)))
  
  
  
  ########################################################################  
  
  for (t in 2:NumYears) {
    
    year[t] = t - 1 
    
    #Beverton-Holt spawner recruit relationship-----
    
    N_at[t, 1] = (0.8 * R0. * h. * Eggs[t - 1]) / (0.2 * phi * (1 - h.) + (h. - 0.2) * Eggs[t - 1])
    
    
    # monthly survival and catch ---- 
    
    for (m in 2:12) {
      
      
      # Introduce fishing as Va*F
      
      
      month_N_at[1, ] = exp(-Mseasonal - Va * F*open[1]) * N_at[(t - 1), ]
      
      
      
      month_N_at[m, ] = exp(-Mseasonal - Va * F*open[m]) * month_N_at[(m - 1), ]
      
      
      
      Catch.monthly[m, ] = (((Va * F* open[m]) / (Va * F*open[m] + Mseasonal)) * month_N_at[m, ] * (1 - exp(-Mseasonal - Va * F*open[m])))*(Weight/1000)
      
      
      Catch.monthly[1, ] = (((Va * F*open[1]) / (Va * F*open[1] + Mseasonal)) * month_N_at[1, ] * (1 - exp(-Mseasonal - Va * F*open[1])))*(Weight/1000)
      
    }
    
    #store results----
    # Annual abundance
    
    N_at[t, 2:maxAge. ] = month_N_at[12, 1:maxAge. -1 ] 
    
    plusGroup = month_N_at[12, maxAge] + month_N_at[12, maxAge - 1]
    
    N_at[t, maxAge] = plusGroup
    
    # Annual catch       
    Catch[t, ] = colSums(Catch.monthly)
    
    CatchTotal[t] = sum(Catch[t, 1:maxAge])
    
    
    # Calculate Egg production for time t----  
    Eggs[t] = sum(N_at[t, ] * fa * Ma * females)
    
    #Calculate Revenue by commercial class ------
    
    Revenue[t, 1] = sum(Catch[t, 1:2] * Price[1] )
    
    Revenue[t, 2] = sum( Catch[t, 3:4] * Price[2] )
    
    Revenue[t, 3] = sum(Catch[t, 5:maxAge.] * Price[3] )
    
    RevenueTotal[t] = sum(Revenue[t, 1:3])
    
    Biomass[t] <- sum(N_at[t, 1:maxAge.] * Weight.)
    
    Profit[t] = RevenueTotal[t] - c*(F)*sum(open)
    
  }
  
  
  
  Months.closed = 12 - months.open[1]
  
  F.Mortality = F * 12
  
  Selectivity = Lc
  
  Out = cbind(Months.closed, F.Mortality, Selectivity, year, N_at, CatchTotal, RevenueTotal, Biomass, Profit)
  
  
  #NPV = (sum(PresentValue(Profit, discount = 0.09, year)))
  
  #final = list(Out, NPV)
  
  return(Out)
}
