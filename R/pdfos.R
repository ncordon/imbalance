#' Probability density function estimation based over-sampling
#'
#' Generate synthetic minority examples for a numerical dataset approximating a
#' Gaussian multivariate distribution which best fits the minority data.
#'
#' To generate the synthetic data it approximates a normal distribution of mean
#' a given example of such class, and the variance of the minority class \eqn{S}
#' multiplied by a parameter which is approximated to minimize Mean Integrated
#' Squared Error of a Gaussian multivariate kernel function.
#'
#' @inheritParams rwo
#'
#' @return A \code{data.frame} with the same structure as \code{dataset},
#'   containing the synthetic examples generated
#' @export
#'
#' @examples
#' data(iris0)
#' set.seed(12345)
#'
#' newSamples <- pdfos(iris0, numInstances = 100)
#'
pdfos <- function(dataset, numInstances, classAttr = "Class"){
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

  # Calcs variance (called covariance in the original algorithm)
  # of the minority class
  minVariance <- stats::var(minority)
  # Try to find an inverse matrix for the positive class, if it exists
  minVarianceInv <- try(solve(stats::var(minority)))
  if(class(minVarianceInv) == "try-error")
    stop(paste("Not a valid method for this dataset.",
               "Variance of the positive class is not an invertible matrix"))

  m <- ncol(minority)
  n <- nrow(minority)

  # Multivariate Gaussian kernel function
  gaussianPDF <- function(bandwidth, vector, minVarianceInv){
    result <- 1/ ((bandwidth ** (-m)) * ((2 * pi) ** (m/2)))
    result * exp(- 1 /(2 * bandwidth ** 2) *
                as.numeric(vector %*% minVarianceInv %*% vector) )
  }


  # Cross validation score function to minimize
  crossValScore <- function(bandwidth){
    value <- 2 * gaussianPDF(bandwidth, 0, 1) / n

    value <- value +
      sum(apply(minority, MARGIN = 1, function(row.i){
        apply(minority, MARGIN = 1, function(row.j){
          evPoint <- as.numeric(row.j - row.i)
          result <- gaussianPDF(sqrt(2) * bandwidth, evPoint, minVarianceInv)
          result - 2 * gaussianPDF(bandwidth, evPoint, minVarianceInv)
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


  # Generate new samples using a normal multivariate distribution with variance
  # myBandwidth² * minVariance
  samples <- minority[sample(1:n, numInstances, replace = T), ]
  newSamples <- t(apply(samples, MARGIN = 1, function(row){
    mvtnorm::rmvnorm(1, mean = data.matrix(row),
                     sigma = myBandwidth * minVariance, method = "chol")
  }))

  # Cleanse newSamples dataset and return them
  .normalizeNewSamples(newSamples, minorityClass, names(minority), classAttr, colTypes)
}
