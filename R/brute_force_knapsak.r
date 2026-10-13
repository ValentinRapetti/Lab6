RNGversion(min(as.character(getRversion()),"3.5.3"))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <- data.frame(w = sample(1:4000, size = n, replace = TRUE),
                               v = runif(n = n, 0, 10000))



brute_force_knapsack <- function(x,W){
  # to do: input validation
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
  output<- list(final_value,final_list)
  names(output)<-c('value','elements')
  return(output)
}