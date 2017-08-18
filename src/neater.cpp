#include <RcppArmadillo.h>
using namespace Rcpp;

//// [[Rcpp::export]]
arma::mat computeGameProfiles(arma::mat probs, arma::uvec knn_neighbours, arma::mat partial_payoffs,
                                  int iterations, double smooth_factor) {
  int num_new_samples = knn_neighbours.n_rows;
  int num_neighbours = knn_neighbours.n_cols;
  //partial_payoffs = trans(partial_payoffs);
  //knn_neighbours = trans(knn_neighbours);
  arma::mat payoff;
  arma::rowvec payoffs(num_new_samples);
  arma::rowvec min_payoffs(num_new_samples);
  arma::rowvec min_probs(num_new_samples);
  arma::rowvec max_probs(num_new_samples);
  arma::mat current_prob;

  for(int i=0; i < iterations; i++){
    // Calculate total payoff for jth new sample
    for(int j=0; j < num_new_samples; j++){
      current_prob = probs.rows(knn_neighbours.row(j));
      payoff = partial_payoffs.row(j) % current_prob * trans(probs.row(num_new_samples + j));
      payoffs(j) = payoff(0,0);
      payoff = partial_payoffs.row(j) % current_prob.col(0);
      min_payoffs = payoff(0,0);
    }

    // Apply time discrete replicator to calculate
    // probability of minority and other classes
    min_probs = (smooth_factor + min_payoffs) / (smooth_factor + payoffs);
    max_probs = 1 - min_probs;
    probs.submat(num_new_samples, 0, probs.n_rows, 0) %= min_probs;
    probs.submat(num_new_samples, 1, probs.n_rows, 1) %= max_probs;
  }

  return probs;
}



// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
timesTwo(42)
*/
