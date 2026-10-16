#'Dynamic knapsack
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
#'@param recursive bool choose which R implementation to use
#'@param rcpp bool choose whether to use C++ implementation
#'@return list object
#'@export
knapsack_dynamic <- function(x,W, recursive=FALSE, fast=FALSE) {
    stopifnot("x needs to be a data frame"=is.data.frame(x),
            "x can only have two columns"=ncol(x)==2, 
            "x needs to contain the weigths (w) and the values (v)"=all(c("w","v") %in% names(x)),
            "W is not an integer"=(is.numeric(W) & length(W)==1),
            "W is negative"=W>0)

    if(fast)
        knapsack_dynamic_Rcpp(x, W)
    else
        knapsack_dynamic_R(x, W, recursive)
}


knapsack_dynamic_R <- function(x,W, recursive=FALSE) {
    # Generate matrix m
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

    # Recursive function for finding all elements in the solution
    if( recursive ){
        recursiveK <- function(i,j){
            if (i==0) return()
            else if (m[i+1,j+1]>m[i,j+1]){
                list <- recursiveK(i-1,j-x$w[i])
                list <- append(list,i)
                return(list)
            }
            else return(recursiveK(i-1,j))
        }
        final_list <- recursiveK(n,W)
    }
    else{
        j <- W+1
        final_list <- c()
        for(i in 0:(n-1)){
            k <- n-i

            if( m[k+1, j] > m[k, j]){
                final_list <- append(final_list, k)
                j <- j-x$w[k]
            }
        }
    }

    final_value <- round(m[n+1,W+1],0)
    

    output <- list(as.integer(final_value), final_list)
    names(output)<-c("value","elements")
    return(output)
}