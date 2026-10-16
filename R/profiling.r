#'Profiling function for the knapsack's algorithms
#'
#'This function prints on screen a profile of the different implemented
#'knapsack algorithms
#'
#' @param n int length of generated data frame
#' @param limit int number of objects (rows) used
#' @param W int maximum weight
#' @param fun char algorithm to profile ["brute", "dynamic", "greedy"]
#' @export
profiling <- function(n=100000, limit=16, W=3500, fun="brute") {
    suppressWarnings( RNGversion(min(as.character(getRversion()),"3.5.3")) )
    set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
    knapsack_objects <- data.frame(w = sample(1:4000, size = n, replace = TRUE),
                                v = runif(n = n, 0, 10000))

    # Start the profiling
    Rprof(tmp <- tempfile(), line.profiling = TRUE, memory.profiling = TRUE, interval = 0.01)
    if( fun=="brute" )
        brute_force_knapsack( knapsack_objects[1:limit, ], W=W )
    else if( fun=="greedy" )
        greedy_knapsack( knapsack_objects[1:limit, ], W=W )
    else if( fun=="dynamic" )
        knapsack_dynamic( knapsack_objects[1:limit, ], W=W )
    else if( fun=="fast" )
        knapsack_dynamic( knapsack_objects[1:limit, ], W=W, fast=TRUE )

    #Finish the profiling
    Rprof(NULL)
    
    print(summaryRprof(tmp, memory = "both", lines="show")[2])
}