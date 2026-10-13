#'knapsack dynamic
#'
#'
#'This function implements a knapsack algorithm using a dynamic programming solution.
#'The function works when the weights are all positive integers.
#'At first the function creates the matrix m whose elements m[i,w] are 
#'the maximum value that can be attained with weight less than or equal to w.
#'Then starting from the matrix returns the list of the elements whose values sum
#'is the maximum and weights sum is less than the maximum weight W  
#'
#'@param x data frame
#'@param W maximum weight
#'@return list object
#'@export 
knapsack_dynamic <- function(x,W){
  stopifnot(is.data.frame(x), ncol(x)==2, all(x$v) >=0, all(x$w)>=0)
  n<-nrow(x)
  m <- matrix(0, nrow = (n+1), ncol = W+1 )
  for (i in 1:n){
    for (j in 1:W){
      if( x$w[i] > j) 
        m[i+1, j+1] <- m[i, j+1]  
      else 
        m[i+1, j+1] <- max(m[i, j+1], m[i, j-x$w[i]+1] + x$v[i])
      }
  }

  recursiveK<- function(i,j){
    if (i==0) return()
    else if (m[i+1,j+1]>m[i,j+1]){
      list <- recursiveK(i-1,j-x$w[i])
      list <- append(list,i)
      return(list)
    }
    else return(recursiveK(i-1,j))
  }
  
  final_value<-round(m[n+1,W+1],0)
  final_list <- recursiveK(n,W)
  output<- list(as.integer(final_value),final_list)
  names(output)<-c('value','elements')
  return(output)
}


RNGversion(min(as.character(getRversion()),"3.5.3"))
Warning in RNGkind("Mersenne-Twister", "Inversion", "Rounding"): non-uniform ’Rounding’
sampler used
##old sampler used for backward compatibility
## suppressWarnings() can be used so that the above warning is not displayed
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <- data.frame(w = sample(1:4000, size = n, replace = TRUE),
                               v = runif(n = n, 0, 10000))










