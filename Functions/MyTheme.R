
MyTheme = function(plot){
  
  
  plot +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank() ,axis.line = element_line(colour = "black")) +
   theme(legend.text = element_text(size = 12), 
            legend.title = element_text(size = 13))+
    theme(axis.text.x = element_text(size=17),
          axis.text.y = element_text(size=17),
          axis.title.x = element_text(size=18),
          axis.title.y = element_text(size=18))
  
  
  
}