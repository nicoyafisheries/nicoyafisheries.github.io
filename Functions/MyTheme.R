
MyTheme = function(plot){
  
  
  plot +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank() ,axis.line = element_line(colour = "black")) +
   theme(legend.text = element_text(size = 15), 
            legend.title = element_text(size = 16))+
    theme(axis.text.x = element_text(size=17),
          axis.text.y = element_text(size=17),
          axis.title.x = element_text(size=18),
          axis.title.y = element_text(size=18))+
     theme(axis.ticks = element_line(colour = "black"), 
            axis.text = element_text(colour = "black"))
  
  
  
  
}