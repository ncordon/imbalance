#' Majority Weighted Minority Oversampling Technique for imbalance dataset
#' learning
#'
#' Modification for SMOTE technique which overcomes some of the problems of the
#' SMOTE technique when there are noisy instances, in which case SMOTE would
#' generate more noisy instances out of them.
#'
#' @param dataset \code{data.frame} to treat. All columns, except
#'   \code{classAttr} one, have to be numeric or coercible to numeric.
#' @param numInstances Integer. Number of new minority examples to generate.
#' @param kNoisy Integer. Parameter of euclidean KNN to detect noisy examples as
#'   those whose whole kNoisy-neighbourhood is from the opposite class.
#' @param kMajority Integer. Parameter of euclidean KNN to detect majority
#'   borderline examples as those who are in any kMajority-neighbourhood of
#'   minority instances. Should be a low integer.
#' @param kMinority Integer. Parameter of euclidean KNN to detect minority
#'   borderline examples as those who are in the KMinority-neighbourhood of
#'   majority borderline ones. It should be a large integer. By default if not
#'   parameter is fed to the function, \eqn{|S^{+}|/2} where \eqn{S^{+}} is the
#'   set of minority examples.
#' @param threshold Numeric. A positive real indicating how much we measure
#'   tolerance of closeness to the boundary of minority boundary examples. A
#'   large integer indicates more margin of distance for a example to be
#'   considerated important boundary one.
#' @param cmax Numeric. A positive real indicating how much we measure tolerance
#'   of closeness to the boundary of minority boundary examples. The larger this
#'   number, the more we are valuing boundary examples.
#' @param cclustering Numeric. A positive real for tuning the output of an
#'   internal clustering. The larger this parameter, the more area focused is
#'   going to be the oversampling.
#' @param classAttr \code{character}. Indicates the class attribute from
#'   \code{dataset}. Must exist in it.
#'
#' @return A \code{data.frame} with the same structure as \code{dataset},
#'   containing the synthetic examples generated.
#' @export
#'
#' @examples
#' data(iris0)
#' set.seed(12345)
#'
#' # Generates new minority examples
#' newSamples <- mwmote(iris0, numInstances = 100, classAttr = "Class")
#'
mwmote <- function(dataset, numInstances, kNoisy = 5, kMajority = 3,
                   kMinority, threshold = 5, cmax = 2, cclustering = 3,
                   classAttr = "Class"){
  checkDataset(dataset, "dataset")
  checkDatasetClass(dataset, classAttr, "dataset")
  colTypes <- .colTypes(dataset, exclude = classAttr)
  dataset <- .convertToNumeric(dataset, exclude = classAttr)
  checkAllColumnsNumeric(dataset, exclude = classAttr, "dataset")

  # Compute minority and majority
  minorityClass <- .whichMinorityClass(dataset, classAttr)
  classes <- dataset[, classAttr]
  dataset <- dataset[, names(dataset) != classAttr]
  attrs <- names(dataset)
  dataset <- data.matrix(dataset)
  minorityIndexes <- which(classes == minorityClass)
  minority <- dataset[minorityIndexes, ]
  majority <- dataset[-minorityIndexes, ]

  # If kMinority is missing, use value as described in the setting of the
  # practical part of the paper
  if(missing(kMinority))
    kMinority <- nrow(minority) / 2

  if(!is.numeric(numInstances) || numInstances <= 0)
    stop("numInstances must be a positive integer")
  if(!is.numeric(kNoisy)  || !is.numeric(kMajority) || !is.numeric(kMinority) ||
     kNoisy <= 0 || kMajority <= 0 || kMinority <= 0)
    stop("kNoisy, kMajority and kMinority must be positive integers")
  if(!is.numeric(threshold)  || !is.numeric(cmax) || !is.numeric(cclustering) ||
     threshold <= 0 || cmax <= 0 || cclustering <= 0)
    stop("threshold, cmax and cclustering must be positive real numbers")


  # Define closeness factor
  dimSize <- ncol(minority)

  closenessFactor <- function(x){
    if(x == 0){
      cmax
    } else{
      min(dimSize/x * cmax/threshold, cmax)
    }
  }

  # Indexes in dataset for kNoisy nearest neighbours (plus 1, we'll have to
  # exclude it) for each minority instances
  cleanMinIndexes <- KernelKnn::knn.index.dist(dataset,
                                               minority,
                                               k = kNoisy + 1,
                                               method = "euclidean")
  cleanMinIndexes <- cleanMinIndexes$test_knn_idx[, -1]

  # Filter noisy examples, i.e. those which haven't got a minority one in
  # their kNoisy neighbourhood

  cleanMinIndexes <- which(apply(cleanMinIndexes, MARGIN = 1, function(row){
    any(row %in% minorityIndexes)
  }))
  filteredMinority <- minority[cleanMinIndexes, ]

  # Find majority borderline and minority borderline instances weighted
  majBorderlineIndexes <- KernelKnn::knn.index.dist(majority,
                                                    filteredMinority,
                                                    k = kMajority,
                                                    method = "euclidean")
  majBorderlineIndexes <- unique(as.vector(majBorderlineIndexes$test_knn_idx))
  majBorderline <- majority[unique(as.vector(majBorderlineIndexes)), ]


  minBorderlineInfo <- KernelKnn::knn.index.dist(minority,
                                                 majBorderline,
                                                 k = kMinority,
                                                 method = "euclidean")

  # Compute weight of selection of each filteredMinority instance
  minBorderlineIndexes <- as.vector(minBorderlineInfo$test_knn_idx)
  informationWeights <- t(apply(minBorderlineInfo$test_knn_dist, MARGIN = 1, function(row){
    row <- sapply(row, closenessFactor)
    (row * row) / sum(row)
  }))
  informationWeights <- as.vector(informationWeights)
  indexesWeighted <- cbind(minBorderlineIndexes, informationWeights)
  selectionWeights <- stats::aggregate(indexesWeighted[, 2],
                                      by = list(index = indexesWeighted[, 1]),
                                      FUN = sum)

  # Average-linkage hierarchical clustering of minority instances
  cleanMinDistances <- as.matrix(stats::dist(filteredMinority))
  sumDists <- sum(apply(cleanMinDistances, MARGIN = 2, function(col){
    min(col[col != 0])
  }))

  thresholdClustering <- sumDists / nrow(filteredMinority) * cclustering
  clusters <- mwmoteCalcClusters(as.matrix(stats::dist(minority)),
                                 thresholdClustering)


  # Generate new samples samples
  xIndexes <- sample(selectionWeights$index, numInstances,
                          replace = T, prob = selectionWeights$x)
  randomNumbers <- stats::runif(numInstances, min = 0, max = 1)

  xs <- minority[xIndexes, ]
  ys <- t(sapply(xIndexes, function(index){
    y <- minority[sample(which(clusters == clusters[index]), size = 1), ]
  }))

  newSamples <- xs + randomNumbers * (ys-xs)

  # Prepare newSamples output
  .normalizeNewSamples(newSamples, minorityClass, attrs, classAttr, colTypes)
}
