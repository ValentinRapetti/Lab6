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