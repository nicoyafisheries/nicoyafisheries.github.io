


AS_map_fun = function(Lc, F, R0. = R0, maxAge. = maxAge ,h. = h, fa. = fa, Ma. = Ma, Length. = Length, Weight. = Weight, M. = M) {
  

  
  #this sets top left
  N_at[1,1] = R0.
  
  
  #  Set inital condition in year t = 0 (assumes no fishing)
  for (a in 2:(maxAge.)) {
    
    N_at[1,a] = N_at[1,(a-1)]*exp(-M.)
    
  }
  
  
  
  #Length (cm) at which a fish is vulnerable to fishing mortality
  
  print(F)
  
  print(Lc)
  
  Va = ifelse(Length. < Lc, 0, 1)
  
  #Eggs produced in year 1
  Eggs[1] = sum(N_at[1,]*fa.*Ma.)
  
  phi <- Eggs[1]
  
  #Catch[1,] = 0
  
  
  for (t in 2:NumYears) {
    
    N_at[t,1] = (0.8 * R0. * h. * Eggs[t-1]) / (0.2 * phi * (1-h.) + (h.-0.2) * Eggs[t-1])
    
    for (a in 2:(maxAge.)) {
      
      
      # Introduce fishing as Va*F  
      N_at[t,a] = exp(-M. - Va[a-1]*F)*N_at[(t - 1),(a-1)]
      
      plusGroup = N_at[t,maxAge] + exp(-M - Va[maxAge]*F)*N_at[(t - 1),(maxAge)]
      
      N_at[t,maxAge] = plusGroup
      
      #Catch by cohort = number of individual age a in year t - indvid. age a captured in year t
      
      # Catch[t,a] = ((Va[a]*F)/(Va[a]*F+M.))*N_at[(t),(a-1)]*(1 - exp(-M. - Va[a]*F))
      # 
      # Catch[t,1] = 0
      # 
    }
    # 
    # Catch$Total[t] = sum(Catch[t,1:maxAge])  
    # 
     Eggs[t] = sum(N_at[t,]*fa*Ma)
    # 
    # #Calculate Revenue by commercial class
    # 
    # Revenue[t,1] = sum(Weight[1:2]*Catch[t,1:2]*Price[1]/1000)
    # 
    # Revenue[t,2] = sum(Weight[3:4]*Catch[t,3:4]*Price[2]/1000)
    # 
    # Revenue[t,3] = sum(Weight[5:max(Age)]*Catch[t,5:max(Age)]*Price[3]/1000)
    # 
    # Revenue[t,4] = sum(Revenue[t,1:3])
    # 
    # 
    # Biomass[t, 1] = N_at[t,1:maxAge]*Weight
  }
  
  
  Out = N_at
  return(Out)
  
}




