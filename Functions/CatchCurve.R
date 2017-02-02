#Linearized Catch Curve using methods from Fish Forever excel
#

# INPUTS -----

# LengthData <- vector of lengths for a single species (Ex: LengthData$Lengths)
# LH <- VB Life history parameters Linf, K, to (can be stored as dataframe or list/)

# OUTPUTS ------

# Z estimate of instantaneous mortality
# Zerror <- 95 5 confidence interval
# p1 <- plot of binned age log frequency with linear fit

CatchCurve <- function(LengthData, LH){

  
    
  # Nested function to calculate mode    
  Mode <- function( values, round = 1){
    
    uniq = unique(round(values, digits = round))
    
    uniq[which.max(tabulate(match(values, uniq)))]
    
    
  }
  
  # Load LH
  
  Linf <- LH$Linf
  
  K <- LH$K
  
  to <- LH$to
  
  # Assign Length data  
  Lengths <- LengthData
  
  #Convert length to age
  Ages <- to - (1/K) * log(1-Lengths/Linf) 
  
  # Use mode to set age at which a fish is fully vulnerable
  
  minAge <- Mode(Ages, 0)
  
  # Bin ages into 1 year bins    
  ageBins = trunc(Ages)
  
  # Create frequency table    
  Age.freq <- plyr::count(ageBins) %>%
    filter(x >= minAge) %>%
    select(Age = x, Count = freq) %>%
    as_data_frame()

  
  
  catch.curve.model <- lm(log(Count) ~ Age, data = Age.freq)
  
  Z <<- (catch.curve.model$coefficients[2])*(-1)
  
  
  CI95 <- confint(catch.curve.model, Age.freq$Age,  level = 0.95)
  
  Zerror <<-( - 1) * (Z + CI95[1])
  
  p1 <<- ggplot(Age.freq, aes( x = Age, y = log(Count))) +
    geom_point()+
    stat_smooth(method = lm) +
    labs(x = 'Age', y = "ln(Age Frequency)")+
    theme_minimal() +
    annotate("text", x = 10, y = 6, label = paste(" Z = ", paste((round(Z, digits = 4)) , round(Zerror, digits = 3), sep = " ± ")))
  
  print(paste(" Z = ", paste((round(Z, digits = 6)))))

}
