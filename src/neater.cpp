#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]]
arma::mat computeGameProfiles(arma::mat probs, arma::umat knn_neighbours,
                              arma::mat partial_payoffs, int iterations,
                              double smooth_factor){
  // Adjust indexes to C++ indexes
  knn_neighbours = knn_neighbours - 1;
  int num_new_samples = knn_neighbours.n_rows;
  int first_new_sample = probs.n_rows - num_new_samples;
  //partial_payoffs = trans(partial_payoffs);
  partial_payoffs = trans(partial_payoffs);
  arma::mat payoff;
  arma::rowvec payoffs(num_new_samples);
  arma::rowvec min_payoffs(num_new_samples);
  arma::rowvec min_probs(num_new_samples);
  arma::mat current_prob;

  for(int i = 0; i < iterations; i++){
    // Calculate total payoff for jth new sample
    for(int j=0; j < num_new_samples; j++){
      current_prob = probs.rows(knn_neighbours.row(j));
      payoff = partial_payoffs.col(j) % (current_prob * trans(probs.row(first_new_sample + j)));
      payoffs(j) = arma::accu(payoff);
      payoff = partial_payoffs.col(j) % current_prob.col(0);
      min_payoffs(j) = arma::accu(payoff);
    }

    // Apply time discrete replicator to calculate
    // probability of minority and majority classes
    min_probs = (smooth_factor + min_payoffs) / (smooth_factor + payoffs);
    probs.submat(first_new_sample, 0, probs.n_rows - 1, 0) %= trans(min_probs);
    probs.submat(first_new_sample, 1, probs.n_rows - 1, 1) = 1 - probs.submat(first_new_sample, 0, probs.n_rows - 1, 0);
  }

  return probs;
}



// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
computeGameProfiles(probs, knnIndexes, partialPayoffs, iterations, smoothFactor)
*/
