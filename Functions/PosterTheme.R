
PosterTheme = function(plot){
  
  
  plot +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank() ,axis.line = element_line(colour = "black")) +
    theme(legend.text = element_text(size = 20), 
          legend.title = element_text(size = 20))+
    theme(axis.text.x = element_text(size=20, colour = "black"),
          axis.text.y = element_text(size=20, colour = "black"),
          axis.title.x = element_text(size=24),
          axis.title.y = element_text(size=24))
 

}