# When there is no standard vector, we put a score for every vector which is in rent range.

score <- function(x, p, h, l, r){
  n <- nrow(x)
  
  for(i in 1:n){
    
    x$score[i] <- (x$park.num[i]/10)^2*(1+p*5) + (x$heal.num[i]*2)^2*(1+h*5) + (x$lib.num[i])^2*(1+l*5) + (x$rest.num[i]/5000)^2*(1+r)
    
  }
  
  x <- x[order(x$score, decreasing = TRUE),]
  
  return(x[1:3,-15])
}