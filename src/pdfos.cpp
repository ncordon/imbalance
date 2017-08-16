#include <RcppArmadillo.h>
#include <numeric>
#include <vector>
using namespace Rcpp;


// Multivariate Gaussian kernel function evaluation
// - point is a column vector
// - covInv is the inverted covariance matrix
//
// We do not output multiplicative constants
double gaussianPDF(arma::colvec &point, arma::mat &covInv){
  arma::mat result = trans(point) * covInv * point;
  return exp(-1/2 * result(0, 0));
}


// Cross validation function as described in the paper, but
// lacking multiplicative constants
double crossValScore(arma::mat &dataset, arma::mat &covInv, double bandwidth){
  arma::colvec point;
  double score = 0;
  int n = dataset.n_cols;
  int d = dataset.n_rows;
  double factor = 1.0 / pow(bandwidth, d);
  double other_factor = 1.0 / pow(sqrt(2), d);
  arma::mat first_cov = 1.0 / (2 * bandwidth * bandwidth) * covInv;
  arma::mat second_cov = 1.0 / (bandwidth * bandwidth) * covInv;

  for(arma::uword i = 0; i < n; i++){
    for(arma::uword j = 0; j < n; j++){
      point = dataset.col(i) - dataset.col(j);
      score += other_factor * gaussianPDF(point, first_cov);
      score -= 2 * gaussianPDF(point, second_cov);
    }
  }

  score /= (n*n);
  score += 2.0/n;
  score *= factor;

  return score;
}

// dataset is the dataset as numeric matrix
// covInv is the inverse of the covariance matrix
//
// [[Rcpp::export]]
double bestGaussianBandwidth(arma::mat &dataset, arma::mat &covInv){
  int n = dataset.n_rows;
  int d = dataset.n_cols;
  dataset = trans(dataset);
  double best_bandwidth;
  double min_cross_val = std::numeric_limits<double>::infinity();
  double current_score;
  double scott_bandwidth = pow(n, (-1.0 / (d + 4)));
  double silverman_bandwidth = pow( 4.0 / (n * (d + 2)), 1.0 / (d + 4));
  std::vector <double> possible_bwidth;
  std::vector<double>::iterator v;

  // Adds Scott's and Silverman's rules of thumb
  possible_bwidth.push_back(scott_bandwidth);
  possible_bwidth.push_back(silverman_bandwidth);

  // Adds grid search values
  for(double v = 0.2 * silverman_bandwidth; v < 1.5 * silverman_bandwidth; v = v + 0.02){
    possible_bwidth.push_back(v);
  }

  // If we improve value of cross validation, update value for bandwidth parameter
  for(v = possible_bwidth.begin(); v != possible_bwidth.end(); ++v){
    current_score = crossValScore(dataset, covInv, *v);

    if(current_score < min_cross_val){
      best_bandwidth = *v;
      min_cross_val = current_score;
    }
  }

  return best_bandwidth;
}


/*** R
bestGaussianBandwidth(minority, covInverse)
*/
