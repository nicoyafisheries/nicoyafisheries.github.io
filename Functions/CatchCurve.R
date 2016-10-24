#Linearized Catch Curve using methods from Fish Forever excel
#
# 

#LengthData <- dataset of lengths for a single species
#
#LH <- VB Life history parameters Linf, K, to 

CatchCurve <- function(LengthData, LH){
  
  Lengths <- LengthData$length_class
  
  Linf <- LH$Linf
  
  K <- LH$K
  
  to <- LH$to
  
  Ages <- to - (1/K) * log(1-Lengths/Linf)
  
  Age.freq <- as.data.frame(table(Ages))
  
  x <- Age.freq$Ages
  
  y <- log(Age.freq$Freq)
  
  
  
  catch.curve.model <- lm(y~x, Age.freq)
  
  Z <- catch.curve.model$coefficients[2]
  
  SE <- 0.01726
  
  ggplot(Age.freq, aes(x= x, y=y)) +
    geom_point()+
    stat_smooth(method = lm) +
    theme_minimal() +
    annotate("text", x= 12, y= 6, label = paste("- Z = ", paste((round(Z, digits = 4)) , SE, sep = " ± ")))
 
print(Z)

return(Age.freq)
   
}