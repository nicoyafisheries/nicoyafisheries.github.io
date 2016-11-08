#Linearized Catch Curve using methods from Fish Forever excel
#
# 

#LengthData <- dataset of lengths for a single species
#
#LH <- VB Life history parameters Linf, K, to 

CatchCurve <- function(LengthData, LH){
 LengthData= corvina.all.2012
  
#calculates Lc value as the mode of data
  uniqv <- unique(LengthData$length_class)
  Lc <- uniqv[which.max(tabulate(match(LengthData$length_class, uniqv)))]
  
  Linf <- LH$Linf
  
  K <- LH$K
  
  to <- LH$to
 
#subsets data to only include lengths >= Lc 
  Lengths <- LengthData$length_class[LengthData$length_class >= Lc]
  
  Ages <- to - (1/K) * log(1-Lengths/Linf)
  
  ageBins = trunc(Ages)
  
  df <- plyr::count(ageBins)
  
  
  x <- df$x
  
  y <- log(df$freq)
  

  catch.curve.model <- lm(log(freq)~x, df)
  
  Z <<- (catch.curve.model$coefficients[2])*(-1)
  
  
  CI95 <- confint(catch.curve.model,x,  level = 0.95)
  
  Zerror <<- Z+CI95[1]
  
p1<<- ggplot(df, aes(x= x, y=log(freq))) +
    geom_point()+
    stat_smooth(method = lm) +
    theme_minimal() +
    annotate("text", x= 10, y= 6, label = paste(" Z = ", paste((round(Z, digits = 4)) , round(Zerror, digits = 3), sep = " ± ")))
 
print(Z)

colnames(df) = c("AgeBins", "Count")

Age.freq <<- df
   
}