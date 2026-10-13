#'brute_force_knapsack
#'
#'
#'This function implements a knapsack algorithm using a brute force approach.
#'It analyses the all the possible combination of objects in the data frame x and find the one with the maximum value
#'and whose weights sum is less than the maximum weight W
#'
#'@param x data frame
#'@param W maximum weight
#'@return list object
#'@export 
brute_force_knapsack <- function(x,W){
  stopifnot(is.data.frame(x), ncol(x)==2, all(x$v) >=0, all(x$w)>=0)
  final_value<- 0
  lista<-c()
  n<- nrow(x)
  for (i in 1:2^n) {
    i<-as.integer(intToBits(i))[1:n]
    weigh<- t(i) %*% x$w
    if (weigh < W){
      value<- t(i) %*% x$v
      if(value > final_value){
        final_value<-value
        lista<-i
      }
    }
    
  }
  final_value<-round(final_value,0)
  final_list <- c()
  for (k in 1:length(lista)) {
    if (lista[k] != 0){
      final_list<-append(final_list, k)
    }
  }
  output<- list(as.integer(final_value),final_list)
  names(output)<-c('value','elements')
  return(output)
}



