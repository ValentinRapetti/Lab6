#'greedy_knapsack
#'
#'
#'This function implements a knapsack algorithm using a greedy approach.
#'Calculates the value, weight ratio of each element in the data frame x and fills the the output set whith the first p
#'elements, ordered according to the ratio, whose weights sum is less than the maximum W
#'
#'@param x data frame
#'@param W maximum weight
#'@return list object
#'@export 
greedy_knapsack<-function(x,W){
  x['id']<-1:nrow(x)
  x['ratio']<- x$v/x$w
  x<-x[order(x$ratio, decreasing = TRUE),]
  wheigh<-0
  value<-0
  lista<-c()
  i<-1
  while (wheigh<W){
    wheigh <- wheigh + x[i,'w']
    value<- value + x[i,'v']
    i <- i+1
  }
  value<-value-x[i-1,'v']
  value<-round(value,0)
  lista<-x$id[1:(i-2)] # why i-2 and not i-1
  output<- list(value,lista)
  names(output)<-c('value','elements')
  return(output)
}