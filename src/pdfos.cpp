#include <RcppArmadillo.h>
#include <numeric>
#include <vector>
using namespace Rcpp;

// Struct that stores score of the CV and differential
struct CrossEvaluation{
  double score;
  double derivative;
};

// Cross validation function that stores both the score of the CV and the derivative
// to be able to perform a gradient descendent
//
// - covInv is the inverted covariance matrix
// - bandwidth is the multiplicative parameter for covariance, resulting in a
//   bandwidth² * cov covariance
CrossEvaluation computeCV(arma::mat &dataset, arma::mat &covInv, double bandwidth){
  CrossEvaluation result;
  arma::colvec point;
  double derivative = 0;
  double score = 0;
  int m = dataset.n_cols, d = dataset.n_rows;
  double factor = 1.0 / pow(bandwidth, d + 1), sqrt_factor = 1.0 / pow(sqrt(2.0), d);
  double first_ev_point, second_ev_point;
  arma::mat first_cov = 1.0 / (2 * bandwidth * bandwidth) * covInv;
  arma::mat second_cov = 1.0 / (bandwidth * bandwidth) * covInv;

  for(arma::uword i = 0; i < m; i++){
    for(arma::uword j = i + 1; j < m; j++){
      point = dataset.col(i) - dataset.col(j);
      arma::mat first_result = -0.5 * trans(point) * first_cov * point;
      first_ev_point = first_result(0,0);
      arma::mat second_result = -0.5 * trans(point) * second_cov * point;
      second_ev_point = second_result(0,0);

      derivative += (-d - 2 * first_ev_point) * factor * sqrt_factor * exp(first_ev_point) * 2;
      derivative -= (-d - 2 * second_ev_point) * factor * exp(second_ev_point) * 4;

      score += 2 * sqrt_factor * exp(first_ev_point);
      score -= 4 * exp(second_ev_point);
    }
  }


  derivative += -d * factor * sqrt_factor * m;
  derivative /= (m * m);
  score += sqrt_factor * m;
  score /= (m * m);
  score *= factor;

  result.score = score;
  result.derivative = derivative;

  return result;
}

// Computes best bandwidth b for a multivariate Gaussian kernel density
// estimation providing that covariance will be b² * original covariance
// of the empirical data
//
// - dataset is the dataset as numeric matrix
// - covInv is the inverse of the covariance matrix
//
// [[Rcpp::export]]
double bestGaussianBandwidth(arma::mat &dataset, arma::mat &covInv){
  // Armadillo works with columns better than with rows
  dataset = trans(dataset);
  int m = dataset.n_cols;
  int d = dataset.n_rows;
  int iterations = 0;
  std::vector <double> possible_bwidth;
  std::vector<double>::iterator v;
  double silverman_bandwidth = pow( 4.0 / (m * (d + 2)), 1.0 / (d + 4));
  double diff;
  double lower_bound = 0.25 * silverman_bandwidth;
  double upper_bound = 1.5 * silverman_bandwidth;
  CrossEvaluation current_eval;
  double best_bandwidth;
  double min_cross_val = std::numeric_limits<double>::infinity();

  // Parameters for gradient descendent
  // Define current bandwidth as Silverman's rule of thumb
  double current_value = silverman_bandwidth;
  double prev_value;
  bool out_bounds = false, first_iteration = true;
  double gamma;
  double precision = 0.0001;
  double previous_step_size = current_value;
  double current_df = -1;


  // Gradient descendent to move in the direction of the derivative of the
  // cross validation function derivative until we cannot improve precision
  // respect to the previous iteration
  while(previous_step_size > precision && !out_bounds && iterations < (m*d)){
    current_eval = computeCV(dataset, covInv, current_value);
    current_df = current_eval.derivative;

    if (current_eval.score < min_cross_val){
      best_bandwidth = current_value;
      min_cross_val = current_eval.score;
    }

    if(first_iteration){
      if(current_df != 0)
        gamma = 0.005 / fabs(current_df);
      else
        gamma = 0.005;

      first_iteration = false;
    }

    prev_value = current_value;
    diff = gamma * current_df;
    current_value -= diff;
    previous_step_size = fabs(current_value - prev_value);
    out_bounds = (current_value < lower_bound || current_value > upper_bound);
    iterations++;
  }

  return best_bandwidth;
}


/*** R
bestGaussianBandwidth(minority, covInverse)
*/
