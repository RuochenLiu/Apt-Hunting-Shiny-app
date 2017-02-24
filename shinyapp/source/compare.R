# Compare vectors

# X is the city dataframe, and V is the vector of current neighborhood. We set up parameters that have better results.

comp <- function(x, v, b){
  
  n <- nrow(x)
  
  br <- b+9 # The house rent - the number of bedroom.
  
  for(i in 1:n){
    
    x$vary[i] <- ((x$park.num[i] - v$park.num)/5)^2 + ((x$heal.num[i] - v$heal.num)/5)^2 + ((x$lib.num[i] - v$lib.num)/5)^2 + ((x$rest.num[i] - v$rest.num)/500)^2 + ((x[i,b+9] - v[1,b+9])/10)^2 +((x$density[i]-v$density)/200)^2
  
  }
  
  x <- x[order(x$vary),]
  
  return(x[1:3,-15])
  
}

