mymode <- function( values, round = 1){
  
  uniq = unique(round(values, digits = round))
                
  uniq[which.max(tabulate(match(values, uniq)))]
  
  
}