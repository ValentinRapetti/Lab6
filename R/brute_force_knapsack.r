#'Brute-force knapsack
#'
#'This function implements a knapsack algorithm using a brute force approach.
#'It analyses the all the possible combination of objects in the data frame 'x'
#'and finds the one with the maximum value which the sum of its weights is less
#'than the maximum weight W.
#'
#'@param x data frame
#'@param W int maximum weight
#'@return list object
#'@export
brute_force_knapsack <- function(x,W){
    stopifnot("x needs to be a data frame"=is.data.frame(x),
              "x can only have two columns"=ncol(x)==2, 
              "x needs to contain the weigths (w) and the values (v)"=all(c("w","v") %in% names(x)),
              "W is not an integer"=(is.numeric(W) & length(W)==1),
              "W is negative"=W>0)

    final_value <- 0
    combination_list <- c()
    n <- nrow(x)

    # Check all the combinations, if the end value is greater
    # we update our results
    for (i in 1:2^n) {
        i <- as.integer(intToBits(i))[1:n]
        weight <- sum(i * x$w)

        if (weight < W){
            value <- t(i) %*% x$v
            if(value > final_value){
                final_value <- value
                combination_list <- i
            }
        }
    }

    # Translate the combinations to idx
    final_value<-round(final_value,0)
    final_list <- c()

    for (k in seq_along(combination_list)) {
        if (combination_list[k] != 0) {
            final_list <- append(final_list, k)
        }
    }

    output<- list(as.integer(final_value), final_list)
    names(output)<-c("value","elements")
    return(output)
}