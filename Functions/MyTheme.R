
MyTheme = function(plot){
  
  
  plot +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank() ,axis.line = element_line(colour = "black")) +
    theme(axis.text.x = element_text(size=17),
          axis.text.y = element_text(size=17),
          axis.title.x = element_text(size=18),
          axis.title.y = element_text(size=18))
  
  
  
}