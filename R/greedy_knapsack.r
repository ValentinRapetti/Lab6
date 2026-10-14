#'Greedy algorithm knapsack
#'
#'This function implements a knapsack algorithm using a greedy approach.
#'Calculates the value-to-weight ratio for each element in the data frame x
#'and fills the the output set with the first p elements, ordered
#'according to the ratio, whose weights sum is less than the maximum W.
#'
#'@param x data frame
#'@param W int maximum weight
#'@return list object
#'@export
greedy_knapsack<-function(x,W){
    stopifnot("x needs to be a data frame"=is.data.frame(x),
              "x can only have two columns"=ncol(x)==2, 
              "x needs to contain the weigths (w) and the values (v)"=all(c("w","v") %in% names(x)),
              "W is not an integer"=(is.numeric(W) & length(W)==1),
              "W is negative"=W>0)

	# Create new columns for x and order the df
    x['id'] <- 1:nrow(x)
    x['ratio'] <- x$v/x$w
    x <- x[order(x$ratio, decreasing = TRUE), ]

	# Start adding elements (greedily)
    weight <- 0
    value <- 0
    i <- 1
    while (weight<W) {
        weight <- weight + x[i, "w"]
        value <- value + x[i, "v"]
        i <- i+1
    }

    value <- round(value-x[i-1,"v"], 0)
    final_list <- x$id[1:(i-2)] # why i-2 and not i-1

    output<- list(value, final_list)
    names(output)<-c("value","elements")
    return(output)
}