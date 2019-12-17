#' Fitering of oversampled data based on non-cooperative game theory
#'
#' Filters oversampled examples from a binary class \code{dataset} using game
#' theory to find out if keeping an example is worthy enough.
#'
#' Uses game theory and Nash equilibriums to calculate the minority examples
#' probability of truly belonging to the minority class. It discards examples
#' which at the final stage of the algorithm have more probability of being a
#' majority example than a minority one.
#'
#' @param dataset The original \code{data.frame}. All columns, except
#'   \code{classAttr} one, have to be numeric or coercible to numeric.
#' @param newSamples A \code{data.frame} containing the samples to be filtered.
#'   Must have the same structure as \code{dataset}.
#' @param k Integer. Number of nearest neighbours to use in KNN algorithm to
#'   rule out samples. By default, 3.
#' @param iterations Integer. Number of iterations for the algorithm. By
#'   default, 100.
#' @param smoothFactor A positive \code{numeric}. By default, 1.
#' @param classAttr \code{character}. Indicates the class attribute from
#'   \code{dataset} and \code{newSamples}. Must exist in them.
#'
#' @return Filtered samples as a \code{data.frame} with same structure as
#'   \code{newSamples}.
#' @export
#'
#' @references
#'
#' Almogahed, B.A.; Kakadiaris, I.A. Neater: Filtering of Over-Sampled Data
#' Using Non-Cooperative Game Theory. Soft Computing 19 (2014), Nr. 11, p.
#' 3301–3322.
#'
#' @examples
#' data(iris0)
#'
#' newSamples <- smotefamily::SMOTE(iris0[,-5], iris0[,5])$syn_data
#' # SMOTE overrides Class attr turning it into class
#' # and dataset must have same class attribute as newSamples
#' names(newSamples) <- c(names(newSamples)[-5], "Class")
#'
#' neater(iris0, newSamples, k = 5, iterations = 100,
#'        smoothFactor = 1, classAttr = "Class")
neater <- function(dataset, newSamples, k = 3, iterations = 100,
                   smoothFactor = 1, classAttr = "Class"){
  checkDataset(dataset)
  checkDataset(newSamples)
  checkDatasetClass(dataset, classAttr)

  if(any(! names(dataset) %in% names(newSamples)) ||
     any(! names(newSamples) %in% names(dataset)))
    stop("dataset and newSamples must have the same structure")

  if(nrow(newSamples) == 0 || nrow(dataset) == 0)
    stop("newSamples and dataset cannot be empty")
  if(!is.numeric(k) || !is.numeric(iterations) ||
     k <= 0 || iterations < 0)
    stop("iterations and k must be positive integers")
  if(!is.numeric(smoothFactor) || smoothFactor <= 0)
    stop("smooth must be a positive number")

  # Extracts shape of the dataset, calcs minority class and instances
  originalShape <- datasetStructure(dataset, classAttr)
  # Compute minority indexes
  minority <- selectMinority(dataset, classAttr)
  minorityIndexes <- whichMinority(dataset, classAttr)
  oldSize <- nrow(dataset)

  # Join dataset and newSamples and strip class attribute
  dataset <- rbind(dataset, newSamples)
  dataset <- dataset[, names(dataset) != classAttr]
  newSamples <- newSamples[, names(newSamples) != classAttr]

  # Convert datasets to numeric
  dataset <- toNumeric(dataset, exclude = classAttr)
  newSamples <- toNumeric(newSamples, exclude = classAttr)
  checkAllColumnsNumeric(newSamples, exclude = classAttr)
  checkAllColumnsNumeric(dataset, exclude = classAttr)

  # Indexes in dataset for k nearest neighbours of each new sample
  knnInfo <- KernelKnn::knn.index.dist(dataset, newSamples,
                                       k = k + 1, method = "euclidean")
  # List with the payoffs for each synthetic sample respect to its
  # k-nearest neighbours
  knnIndexes <- knnInfo$test_knn_idx[, -1, drop = FALSE]
  partialPayoffs <- apply(knnInfo$test_knn_dist[, -1, drop = FALSE],
                          MARGIN = c(1,2),
                          function(x) 1/(x**2 + 1))

  # Matrix of probabilities of belonging to each class, with
  # 1 == minority class. Samples are tagged with probability
  # 0.5 for both classes if they belong to newSamples. Otherwise,
  # they are assigned whole 1 probability to their original class
  probs <- matrix(nrow = nrow(dataset), ncol = 2)
  probs[ minorityIndexes, 1] <- 1
  probs[ minorityIndexes, 2] <- 0
  probs[-minorityIndexes, 1] <- 0
  probs[-minorityIndexes, 2] <- 1
  probs[(oldSize + 1):nrow(dataset), ] <- 0.5

  # compute iterations update of the profile strategies probabilities
  probs <- computeGameProfiles(probs, knnIndexes, partialPayoffs, iterations, smoothFactor)

  # Select synthetic instances whose probabily of belonging
  # to the minority class is greater than a half
  badSamples <-  which(probs[(oldSize+1):nrow(probs), 1] <= 0.5)
  print(paste(length(badSamples), "samples filtered by NEATER"))

  if(length(badSamples) > 0)
    newSamples <- newSamples[-badSamples, ]

  # Append class column to minority samples and return them
  normalizeNewSamples(originalShape, newSamples)
}
