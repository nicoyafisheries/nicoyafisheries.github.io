#Linearized Catch Curve using methods from Fish Forever excel
#
# 

#LengthData <- dataset of lengths for a single species
#
#LH <- VB Life history parameters Linf, K, to 

CatchCurve <- function(LengthData, LH, ageBins){
  LengthData= corvina.all.2012
  
  Lengths <- LengthData$length_class
  
  Linf <- LH$Linf
  
  K <- LH$K
  
  to <- LH$to
  
  Ages <- to - (1/K) * log(1-Lengths/Linf)
  
  ageBins = 1
  
  br <- seq(from = round(min(Ages)), to=(max(Ages)+1), by = ageBins)
  
  #Age.freq <- as.data.frame(table(Ages)) %>%
    freq   = hist(Ages, breaks = br, include.lowest=TRUE, plot=FALSE)
  
  x <- freq$Ages
  
  y <- log(freq$counts)
  
Age.freq<-  data.frame(Age = head(br,-1), 
                      lnCounts = log(freq$counts))
  
  catch.curve.model <- lm(Counts~Age, Age.freq)
  
  Z <- catch.curve.model$coefficients[2]
  
  SE <- 0.01726
  
p1<<- ggplot(Age.freq, aes(x= Age, y=Counts)) +
    geom_point()+
    stat_smooth(method = lm) +
    theme_minimal() +
    annotate("text", x= 12, y= 6, label = paste("- Z = ", paste((round(Z, digits = 4)) , SE, sep = " ± ")))
 
print(Z)

Age.freq <<- Age.freq
   
}