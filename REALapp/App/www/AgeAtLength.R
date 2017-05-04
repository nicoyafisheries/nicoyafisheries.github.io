
AgeAtLength <- function(Lengths,to, K, Linf) {
  
  Age = to - (1/K) * log(1- Lengths /Linf)

  
  return(Age)
}