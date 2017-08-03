#' Probability density function estimation based over-sampling
#'
#' @param dataset \code{data.frame} to treat
#' @param numInstances Integer. Number of new minority examples to generate.
#' @param classAttr String. Indicates the class attribute from \code{dataset}.
#'   Must exist in it.
#'
#' @return new samples, a \code{data.frame} with the same structure as
#'   \code{dataset}, containing the synthetic examples generated
#' @export
#'
#' @examples
pdfos <- function(dataset, numInstances, classAttr = "Class"){
  if(!is.data.frame(dataset))
    stop("dataset must be a data.frame")
  if(!classAttr %in% names(dataset))
    stop("class attribute not found in dataset")
  if(!is.numeric(numInstances) || numInstances < 0)
    stop("numInstances must be a positive integer")

  # Calcs minority class and instances
  minorityClass <- .whichMinorityClass(dataset, classAttr)
  minority <- dataset[dataset[, classAttr] == minorityClass,
                      names(dataset) != classAttr]

  # Calcs variance (called covariance in the original algorithm)
  # of the positive class
  minorityVar <- stats::var(minority)
  m <- ncol(minority)
  n <- nrow(minority)

  # Kernel function
  phi <- function(sigma, vector, variance = 1){
    result <- (sigma ** (-m)) / ((2 * pi) ** (m/2))
    result * exp(- (1/2 * (sigma ** 2)) *
                as.numeric(vector %*% variance**(-1) %*% vector) )
  }


  # Cross validation score function to minimize
  crossValScore <- function(sigma){
    value <- 2 * phi(sigma, 0)

    value <- value +
      sum(apply(minority, MARGIN = 1, function(row.i){
        apply(minority, MARGIN = 1, function(row.j){
          evPoint <- as.numeric(row.j - row.i)
          phi(sqrt(2) * sigma, evPoint, minorityVar) -
            2 * phi(sigma, evPoint, minorityVar)
        })
      })) / n

    value / n
  }


  # grid search to find approximation to best sigma parameter
  gridParams <- seq(from = 0, to = 2, by = 0.05)
  # erase 0 from grid search
  grid <- gridParams[2:length(gridParams)]
  minIndex <- which.min( sapply(gridParams, function(x){ crossValScore(x) }) )

  mySigma <- gridParams[ minIndex ]

  # Choleski form of variance matrix, upper triangular matrix
  varCholeski <- chol(minorityVar)

  # Generate new samples using a normal multivariate distribution with variance
  # mySigma² * minorityVar
  samples <- minority[sample(1:n, numInstances, replace = T), ]
  newSamples <- apply(samples, MARGIN=1, function(row){
    row + t( mySigma * varCholeski %*% stats::rnorm( m, mean = 0, sd = 1) )
  })

  # Cleanse newSamples dataset and output it
  newSamples <- t(newSamples)
  newSamples <- data.frame(newSamples)
  .normalizeNewSamples(newSamples, minorityClass, names(minority), classAttr)
}
