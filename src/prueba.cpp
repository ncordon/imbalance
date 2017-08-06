#include <Rcpp.h>
#include <string>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector racog_(DataFrame dataset, IntegerMatrix edges, int root, int iterations, int burnin, int lag){

  // for(int i=0; i < edges.nrow(); i++)
  //   for(int j=0; j < edges.ncol(); j++)
  //     Rcout << edges(i,j);

  return dataset["PetalLength"];
}
