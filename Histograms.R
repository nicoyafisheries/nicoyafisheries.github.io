load('lengths.RData')

library(tidyverse)

quick = filter(lengths, Zone == 201)

ggplot(quick, aes(Length)) +
  geom_histogram( binwidth = 1)
  
p = ggplot()+
  geom_histogram(data = quick[quick$Year =="2012", ],  aes(Length, fill = "2012"), binwidth = 1, alpha = 0.8) +
  geom_histogram(data = quick[quick$Year =="2014", ],  aes(Length, fill = "2014"), binwidth = 1, alpha = 0.6)
  

plotly::ggplotly(p)