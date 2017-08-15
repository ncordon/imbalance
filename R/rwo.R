#' Random Walk Oversampling
#'
#' Generate synthetic minority examples for a dataset trying to preserve the
#' variance and mean of the minority class. Works on every type of dataset.
#'
#' Generates \code{numInstances} new minority examples for \code{dataset},
#' adding to the each column of the j-th example its variance scalated by the
#' inverse of the number of minority examples and a factor following a N(0,1)
#' distribution which depends on the example.
#'
#' @param dataset \code{data.frame} to treat. All columns, except
#'   \code{classAttr} one, have to be numeric or coercible to numeric.
#' @param numInstances Integer. Number of new minority examples to generate.
#' @param classAttr String. Indicates the class attribute from \code{dataset}.
#'   Must exist in it.
#'
#' @return A \code{data.frame} with the same structure as \code{dataset},
#'   containing the synthetic examples generated
#' @export
#'
#' @examples
#' data(iris0)
#' set.seed(12345)
#'
#' newSamples <- rwo(iris0, numInstances = 100, classAttr = "Class")
#'
rwo <- function(dataset, numInstances, classAttr = "Class"){
  checkDataset(dataset, "dataset")
  checkDatasetClass(dataset, classAttr, "dataset")
  colTypes <- .colTypes(dataset, exclude = classAttr)
  dataset <- .convertToNumeric(dataset, exclude = classAttr)
  checkAllColumnsNumeric(dataset, exclude = classAttr, "dataset")
  if(!is.numeric(numInstances) || numInstances <= 0)
    stop("numInstances must be a positive integer")

  # Calcs minority class and instances
  minorityClass <- .whichMinorityClass(dataset, classAttr)
  minority <- dataset[dataset[, classAttr] == minorityClass,
                      names(dataset) != classAttr]

  m <- nrow(minority)

  if(nrow(minority) > 0){
    iterPerInstance <- ceiling(numInstances / nrow(minority))
    # Multiplicative factors following a normal distribution that depend
    # on each example
    scaleFactors <- stats::rnorm(nrow(minority) * iterPerInstance, mean = 0, sd = 1)
  }

  newSamples <- lapply(minority, function(x){
    # If attribute is continuous, generate new minority sample preserving
    # mean and variance of existent samples
    if(class(x) == "numeric"){
      variance <- stats::var(x)
      x - variance/sqrt(m) * scaleFactors

    # Else if attribute is not numeric, make a roulette out of possible
    # values for the attribute and their frequency
    } else{
      dist <- table(x)
      distValues <- names(dist)
      distProbs <- unname(dist)
      sample(distValues, length(x) * iterPerInstance, replace = T, prob = distProbs)
    }
  })

  # Select numInstances randomly (if we have generated more instances than
  # required for each minority example) and output them
  newSamples <- data.frame(newSamples)
  indexes <- sample(1:nrow(newSamples), numInstances, replace = F)
  newSamples <- newSamples[indexes, ]
  .normalizeNewSamples(newSamples, minorityClass, names(minority), classAttr, colTypes)
}
