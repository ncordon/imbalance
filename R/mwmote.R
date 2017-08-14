

mwmote <- function(dataset, numInstances, kNoisy = 5, kMajority = 3,
                   kMinority, threshold = 5, cmax = 2, cclustering = 3,
                   classAttr = "Class"){
  checkDataset(dataset, "dataset")
  checkDatasetClass(dataset, classAttr, "dataset")
  colTypes <- .colTypes(dataset, exclude = classAttr)

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

  if(!is.numeric(kNoisy)  || !is.numeric(kMajority) || !is.numeric(kMinority) ||
     kNoisy <= 0 || kMajority <= 0 || kMinority <= 0)
    stop("kNoisy, kMajority and kMinority must be positive integers")


  # Define closeness factor
  dimSize <- ncol(minority)

  closenessFactor <- function(x){
    if(x == 0){
      cmax
    } else{
      min(dimSize/x * cmax/threshold, cmax)
    }
  }

  # Compute feature space size
  # Indexes in dataset for kNoisy nearest neighbours (plus 1, we'll have to exclude it)
  # for each minority instances
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
  cleanMinority <- minority[cleanMinIndexes, ]

  # Find majority borderline and minority borderline instances weighted
  majBorderlineIndexes <- KernelKnn::knn.index.dist(majority,
                                                    cleanMinority,
                                                    k = kMajority,
                                                    method = "euclidean")
  majBorderlineIndexes <- unique(as.vector(majBorderlineIndexes$test_knn_idx))
  majBorderline <- majority[unique(as.vector(majBorderlineIndexes)), ]


  minBorderlineInfo <- KernelKnn::knn.index.dist(cleanMinority,
                                                    majBorderline,
                                                    k = kMinority,
                                                    method = "euclidean")

  # Compute weight of selection of each cleanMinority instance
  minBorderlineIndexes <- as.vector(minBorderlineInfo$test_knn_idx)
  selectionWeights <- t(apply(minBorderlineInfo$test_knn_dist, MARGIN = 1, function(row){
    row <- sapply(row, closenessFactor)
    (row * row) / sum(row)
  }))
  selectionWeights <- as.vector(selectionWeights)
  indexesWeighted <- cbind(minBorderlineIndexes, selectionWeights)
  selectionWeights <- stats::aggregate(indexesWeighted[, 2],
                                      by = list(index = indexesWeighted[, 1]),
                                      FUN = sum)

  # Average-linkage hierarchical clustering of minority instances
  cleanMinDistances <- as.matrix(dist(cleanMinority))
  sumDists <- sum(apply(cleanMinDistances, MARGIN = 2, function(col){
    min(col[col != 0])
  }))

  thresholdClustering <- sumDists / nrow(cleanMinority) * cclustering
  clusters <- mwmoteCalcClusters(as.matrix(dist(minority)), thresholdClustering)


  # Generate new samples samples
  cleanSelected <- sample(selectionWeights$index, numInstances,
                          replace = T, prob = selectionWeights$x)
  randomNumbers <- runif(numInstances, min = 0, max = 1)

  xIndexes <- cleanMinIndexes[cleanSelected]
  xs <- minority[xIndexes, ]
  ys <- t(sapply(xIndexes, function(index){
    y <- minority[sample(which(clusters == clusters[index]), size = 1), ]
  }))

  newSamples <- xs + randomNumbers * (ys-xs)

  # Prepare newSamples output
  .normalizeNewSamples(newSamples, minorityClass, attrs, classAttr, colTypes)
}
