knapsack_dynamic <- function(x,W){
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













