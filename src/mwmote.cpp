#include <RcppArmadillo.h>
#include <vector>
#include <numeric>
#include <algorithm>
using namespace Rcpp;

// Function to find the clusters (different clusters) that materialize the minimum distance
// distance_matrix is considered to be an upper triangular matrix
void updateNearestClusters(arma::mat &distance_matrix, double &min_distance, arma::uword &min_row, arma::uword &min_col){
  min_distance = std::numeric_limits<double>::infinity();

  for(int i = 0; i < distance_matrix.n_rows; i++){
    for(int j = i + 1; j < distance_matrix.n_cols; j++){
      //Rcout << "i: " << i << "j: " << j << std::endl;
      if(distance_matrix(i,j) < min_distance){
        min_distance = distance_matrix(i,j);
        min_row = i;
        min_col = j;
      }
    }
  }
}

// Updates matrix by selecting min of distance to i and j, that are clusters to be merged, for each
// other cluster
// precondition: merge_i < merje_j
void updateDistanceMatrix(arma::mat &distance_matrix, arma::uword &merge_i, arma::uword &merge_j){
  for(int i = 0; i < merge_i; i++)
    distance_matrix(i, merge_i) = std::min(distance_matrix(i, merge_i), distance_matrix(i, merge_j));

  for(int j = merge_i + 1; j < distance_matrix.n_cols; j++)
    distance_matrix(merge_i, j) = std::min(distance_matrix(merge_i, j), distance_matrix(merge_j, j));

  // Erase column and row corresponding to jth cluster
  distance_matrix.shed_row(merge_j);
  distance_matrix.shed_col(merge_j);
}



// For a given distance matrix among instances, and a threshold, makes a hierarchical
// clustering that stops when minimum distance between clusters is greater than
// threshold, or we run out of clusters to merge.
// [[Rcpp::export]]
IntegerVector mwmoteCalcClusters(arma::mat &distance_matrix, double threshold) {
  std::vector<int> clusters(distance_matrix.n_rows);
  std::vector<int> indexes(distance_matrix.n_rows);
  std::iota(clusters.begin(), clusters.end(), 0);
  std::iota(indexes.begin(), indexes.end(), 0);
  double min_distance = std::numeric_limits<double>::infinity();
  arma::uword min_row = 0, min_col = 0;
  //
  // Update min distance untill the moment and position and clusters that materialize that distance
  updateNearestClusters(distance_matrix, min_distance, min_row, min_col);


  // While we meet the threshold condition and there still are clusters to merge
  while(min_distance <= threshold && distance_matrix.n_rows > 1){
    // Merge two clusters
    clusters[indexes[min_col]] = min_row;

    for(int i = min_col; i < indexes.size(); i++)
      indexes[i]++;

    updateDistanceMatrix(distance_matrix, min_row, min_col);
    updateNearestClusters(distance_matrix, min_distance, min_row, min_col);
  }

  return IntegerVector(clusters.begin(), clusters.end());
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
mwmoteCalcClusters(minDistances, thresholdClustering)
*/
