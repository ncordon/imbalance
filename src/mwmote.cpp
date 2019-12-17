#include <RcppArmadillo.h>
#include <vector>
#include <list>
#include <numeric>
using namespace Rcpp;

// Class that stores
// - the clusters as 'buckets'
// - the distance matrix between clusters
// - the number of points
// - the minimum distance between clusters
// - the number of points clustered
class HierarchicalClustering{
private:
  std::vector< std::list<int> > clusters;
  arma::mat distance_matrix;
  double min_distance;
  int num_points;
public:
  // Initializes cluster with distances, num of points to perform the clustering
  // and num_points clusters, each of a single point
  HierarchicalClustering(arma::mat &dists){
    double current_dist;

    for(int i = 0; i < dists.n_cols; i++){
      std::list<int> cluster;
      cluster.push_back(i);
      clusters.push_back(cluster);
    }

    min_distance = std::numeric_limits<double>::infinity();
    distance_matrix = dists;
    num_points = distance_matrix.n_rows;

    for(int i = 0; i < num_points; i++){
      for(int j = i + 1; j < distance_matrix.n_cols; j++){
        current_dist = distance_matrix(i,j);
        if(current_dist < min_distance)
          min_distance = current_dist;
      }
    }
  }

  // Updates matrix by selecting min of distance to i and j, that are clusters to be merged, for each
  // other cluster
  // precondition: merge_i < merje_j
  void mergeNearestClusters(){
    double distance = std::numeric_limits<double>::infinity();
    double current_dist;
    arma::uword cluster_i, cluster_j;

    // Compute nearest clusters and distance between them
    for(int i = 0; i < distance_matrix.n_rows; i++){
      for(int j = i + 1; j < distance_matrix.n_cols; j++){
        current_dist = distance_matrix(i,j) / (clusters[i].size() * clusters[j].size());
        if(current_dist < distance){
          distance = current_dist;
          cluster_i = i;
          cluster_j = j;
        }
      }
    }

    min_distance = distance;

    // Update distance matrix
    for(int i = 0; i < cluster_i; i++)
      distance_matrix(i, cluster_i) = distance_matrix(i, cluster_i) + distance_matrix(i, cluster_j);

    for(int j = cluster_i + 1; j < distance_matrix.n_cols; j++)
      distance_matrix(cluster_i, j) = distance_matrix(cluster_i, j) + distance_matrix(cluster_j, j);

    // Erase column and row corresponding to jth cluster
    distance_matrix.shed_row(cluster_j);
    distance_matrix.shed_col(cluster_j);
    
    // Merge clusters
    for (auto elem : clusters[cluster_j])
      clusters[cluster_i].push_back(elem);
    
    clusters.erase(clusters.begin() + cluster_j);
  }

  double getMinDistance(){
    return min_distance;
  }

  int getNumClusters(){
    return clusters.size();
  }

  // Get a cluster assination of the form
  // 0 0 0 0 1 1 0 0 1 1 0 0 2 0 0 0 0 3 4
  std::vector<int> getClusterAssignation(){
    std::vector<int> assignation(num_points);

    for(int i=0; i < clusters.size(); i++){
      for(std::list<int>::iterator j = clusters[i].begin(); j != clusters[i].end(); ++j)
        assignation[*j] = i;
    }

    return assignation;
  }
};


// For a given distance matrix among instances, and a threshold, makes a hierarchical
// clustering that stops when minimum distance between clusters is greater than
// threshold, or we run out of clusters to merge.
// [[Rcpp::export]]
IntegerVector hClustering(arma::mat &distance_matrix, double threshold) {
  HierarchicalClustering clustering(distance_matrix);

  // While we meet the threshold condition and there still are clusters to merge
  while(clustering.getMinDistance() <= threshold && clustering.getNumClusters() > 1){
    // Merge two clusters
    clustering.mergeNearestClusters();
  }

  std::vector<int> clusters = clustering.getClusterAssignation();
  return IntegerVector(clusters.begin(), clusters.end());
}


/*** R
mwmoteCalcClusters(minDistances, thresholdClustering)
*/
