#include <Rcpp.h>
#include <algorithm>
#include <cmath>

using namespace Rcpp;

// [[Rcpp::export]]
List knapsack_dynamic_Rcpp(DataFrame x, int W){
    int n= x.nrows(), i, j, final_value;
    NumericMatrix m( (n+1), (W+1) );
    List output;
    IntegerVector final_list;
    NumericVector w=x["w"], v=x["v"];

    // Initialize first row to 0
    for (j = 0; j <= W; j++) {
        m(0, j) = 0;
    }

    // Initialize first column to 0
    for (i = 0; i <= n; i++) {
        m(i, 0) = 0;
    }

    for(i=1; i<=n; i++){
        for(j=1; j<=W; j++){
            if( w[i-1]>j ){
                m(i, j) = m(i-1, j);
            }
            else{
                m(i, j) = std::max( m(i-1, j), m(i-1, j-w(i-1)) + v(i-1));
            }
        }
    };

    j = W;
    for(i=n; i>=1; i--){
        if( m(i, j) > m(i-1, j)){
            final_list.push_back(i);
            j -= w(i-1);
        }
    };

    final_value = std::round( m(n, W));

    // return(final_list);
    return List::create(
    Named("value") = final_value,
    Named("elements") = final_list
    );
}