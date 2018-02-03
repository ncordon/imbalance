#' Random walk oversampling
#'
#' Generates synthetic minority examples for a dataset trying to preserve the
#' variance and mean of the minority class. Works on every type of dataset.
#'
#' Generates \code{numInstances} new minority examples for \code{dataset},
#' adding to the each numeric column of the j-th example its variance scalated
#' by the inverse of the number of minority examples and a factor following a
#' \eqn{N(0,1)} distribution which depends on the example. When the column is
#' nominal, it uses a roulette scheme.
#'
#' @param dataset \code{data.frame} to treat. All columns, except
#'   \code{classAttr} one, have to be numeric or coercible to numeric.
#' @param numInstances Integer. Number of new minority examples to generate.
#' @param classAttr \code{character}. Indicates the class attribute from
#'   \code{dataset}. Must exist in it.
#'
#' @return A \code{data.frame} with the same structure as \code{dataset},
#'   containing the generated synthetic examples.
#' @export
#'
#' @references
#'
#' Zhang, Huaxiang; Li, Mingfang. Rwo-Sampling: A Random Walk Over-Sampling
#' Approach To Imbalanced Data Classification. Information Fusion 20 (2014), p.
#' 99–116.
#'
#' @examples
#' data(iris0)
#'
#' newSamples <- rwo(iris0, numInstances = 100, classAttr = "Class")
#'
rwo <- function(dataset, numInstances, classAttr = "Class"){
  checkDataset(dataset)
  checkDatasetClass(dataset, classAttr)
  originalShape <- datasetStructure(dataset, classAttr)
  dataset <- toNumeric(dataset, exclude = classAttr)
  checkAllColumnsNumeric(dataset, exclude = classAttr)
  if(!is.numeric(numInstances) || numInstances <= 0)
    stop("numInstances must be a positive integer")

  # Calcs minority class and instancess
  minority <- selectMinority(dataset, classAttr)
  minority <- data.matrix(minority)

  m <- nrow(minority)

  if(nrow(minority) > 0){
    iterations <- ceiling(numInstances / nrow(minority))
  }

  newSamples <- apply(minority, MARGIN = 2, function(x){
    # If attribute is continuous, generate new minority sample preserving
    # mean and variance of existent samples
    scaleFactors <- stats::rnorm(nrow(minority) * iterations, mean = 0, sd = 1)

    if(class(x) == "numeric"){
      variance <- (m-1)/m * stats::var(x)
      x - variance/sqrt(m) * scaleFactors

    # Else if attribute is not numeric, make a roulette out of possible
    # values for the attribute and their frequency
    } else{
      dist <- table(x)
      distValues <- names(dist)
      distProbs <- unname(dist)
      sample(distValues, length(x) * iterations, replace = T, prob = distProbs)
    }
  })


  newSamples <- selectSamples(newSamples, numInstances)
  normalizeNewSamples(originalShape, newSamples)
}
