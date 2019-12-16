#' Probability density function estimation based oversampling
#'
#' Generates synthetic minority examples for a numerical dataset approximating a
#' Gaussian multivariate distribution which best fits the minority data.
#'
#' To generate the synthetic data, it approximates a normal distribution with
#' mean a given example belonging to the minority class, and whose variance is
#' the minority class variance multiplied by a constant; that constant is
#' computed so that it minimizes the mean integrated squared error of a Gaussian
#' multivariate kernel function.
#'
#' @inheritParams rwo
#'
#' @return A \code{data.frame} with the same structure as \code{dataset},
#'   containing the generated synthetic examples.
#' @export
#'
#' @references
#'
#' Gao, Ming; Hong, Xia; Chen, Sheng; Harris, Chris J.; Khalaf, Emad. Pdfos: Pdf
#' Estimation Based Oversampling for Imbalanced Two-Class Problems.
#' Neurocomputing 138 (2014), p. 248–259
#'
#' Silverman, B. W. Density Estimation for Statistics and Data Analysis. Chapman
#' & Hall, 1986. – ISBN 0412246201
#'
#' @examples
#' data(iris0)
#'
#' newSamples <- pdfos(iris0, numInstances = 100, classAttr = "Class")
#'
pdfos <- function(dataset, numInstances, classAttr = "Class"){
  checkDataset(dataset)
  checkDatasetClass(dataset, classAttr)
  originalShape <- datasetStructure(dataset, classAttr)
  dataset <- toNumeric(dataset, exclude = classAttr)
  checkAllColumnsNumeric(dataset, exclude = classAttr)

  if(!is.numeric(numInstances) || numInstances <= 0)
    stop("numInstances must be a positive integer")

  # Calcs minority class and instances
  minority <- selectMinority(dataset, classAttr)
  minority <- data.matrix(minority)

  # Computes covariance of the minority class
  covariance <- stats::var(minority)

  # Try to find an inverse matrix for the positive class, if it exists
  covInverse <- try(solve(covariance))
  if(inherits(covInverse, "try-error"))
    stop(paste("Not a valid method for this dataset.",
               "Variance of the positive class is not an invertible matrix"))

  # Find best value for bandwidth
  myBandwidth <- bestGaussianBandwidth(minority, covInverse)

  # Generate new samples using a normal multivariate distribution with covariance
  # myBandwidth² * covariance
  samples <- minority[sample(1:nrow(minority), numInstances, replace = T), ]
  newSamples <- t(apply(samples, MARGIN = 1, function(row){
    mvtnorm::rmvnorm(1, mean = data.matrix(row),
                     sigma = myBandwidth * covariance, method = "chol")
  }))

  # Cleanse newSamples dataset and return them
  normalizeNewSamples(originalShape, newSamples)
}
