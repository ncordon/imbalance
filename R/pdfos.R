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
#' data(iris0)
#'
#' newSamples <- pdfos(iris0, numInstances = 100)
#'
pdfos <- function(dataset, numInstances, classAttr = "Class"){
  if(!is.data.frame(dataset))
    stop("dataset must be a data.frame")
  if(!classAttr %in% names(dataset))
    stop("class attribute not found in dataset")
  if(any(! .colTypes(dataset, exclude = classAttr) == "numeric"))
    stop("all columns of dataset must be numeric")
  if(!is.numeric(numInstances) || numInstances < 0)
    stop("numInstances must be a positive integer")

  # Calcs minority class and instances
  minorityClass <- .whichMinorityClass(dataset, classAttr)
  minority <- dataset[dataset[, classAttr] == minorityClass,
                      names(dataset) != classAttr]

  # Calcs variance (called covariance in the original algorithm)
  # of the positive class
  minVariance <- stats::var(minority)
  # Try to find an inverse matrix for the positive class, if it exists
  minVarianceInv <- try(solve(stats::var(minority)))
  if(class(minVariance) == "try-error")
    stop(paste("Not a valid method for this dataset.",
               "Variance of the positive class is not an invertible matrix"))

  m <- ncol(minority)
  n <- nrow(minority)

  # Multivariate Gaussian kernel function
  phi <- function(bandwidth, vector, minVarianceInv){
    result <- 1/ ((bandwidth ** (-m)) * ((2 * pi) ** (m/2)))
    result * exp(- 1 /(2 * bandwidth ** 2) *
                as.numeric(vector %*% minVarianceInv %*% vector) )
  }


  # Cross validation score function to minimize
  crossValScore <- function(bandwidth){
    value <- 2 * phi(bandwidth, 0, 1) / n

    value <- value +
      sum(apply(minority, MARGIN = 1, function(row.i){
        apply(minority, MARGIN = 1, function(row.j){
          evPoint <- as.numeric(row.j - row.i)
          result <- phi(sqrt(2) * bandwidth, evPoint, minVarianceInv)
          result - 2 * phi(bandwidth, evPoint, minVarianceInv)
        })
      })) / (n*n)

    value
  }


  # grid search to find approximation to best bandwidth parameter
  gridParams <- seq(from = 0.2, to = 2, by = 0.02)
  # adds to grid breaks Scott's and Silverman's rules of thumb
  gridParams <- c(gridParams,
                  n ** (-1 / (m + 4)),
                  (4 / (n * (m + 2))) ** (1 / (m + 4)) )
  minIndex <- which.min( sapply(gridParams, function(x){ crossValScore(x) }) )

  myBandwidth <- gridParams[ minIndex ]

  # Choleski form of variance matrix, upper triangular matrix
  varCholeski <- chol(minVariance)

  # Generate new samples using a normal multivariate distribution with variance
  # myBandwidth² * minVariance
  samples <- minority[sample(1:n, numInstances, replace = T), ]
  newSamples <- apply(samples, MARGIN=1, function(row){
    row + t( myBandwidth * varCholeski %*% stats::rnorm( m, mean = 0, sd = 1) )
  })

  # Cleanse newSamples dataset and output it
  newSamples <- t(newSamples)
  newSamples <- data.frame(newSamples)
  .normalizeNewSamples(newSamples, minorityClass, names(minority), classAttr)
}
