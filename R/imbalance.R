#' imabalance: A package to treat imbalanced datasets
#'
#' Focused on binary class datasets, the \code{imbalance} package provides
#' methods to generate synthetic examples and achieve balance between the
#' minority and majority classes in dataset distributions
#'
#' @section Oversampling functions for the minority class: \code{\link{racog}},
#'   \code{\link{wracog}}, \code{\link{rwo}}, \code{\link{pdfos}}.
#'
#' @section Methods to visually evaluate algorithms:
#'   \code{\link{plotComparison}}.
#'
#' @section Methods to filter oversampled instances \code{\link{neater}}.
#'
#' @docType package
#' @name imabalace
NULL

#' @useDynLib imbalance, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL


#' Wrapper that encapsulates a collection of algorithms to perform a class
#' balancing preprocessing task for binary class datasets
#'
#' @param dataset A binary class \code{data.frame} to balance.
#' @param ratio Number between 0 and 1 indicating the desired ratio between
#'   minority examples and majority ones, that is, the quotient
#'   \eqn{\frac{|minority examples|}{|majority examples|}}
#' @param method A \code{character} corresponding to method to apply. Possible
#'   methods are: \code{\link{racog}}, \code{\link{wracog}}, \code{\link{rwo}},
#'   \code{\link{pdfos}}, \code{\link{mwmote}},
#'   \code{\link[smotefamily]{ADASYN}}, \code{\link[smotefamily]{ANS}},
#'   \code{\link[smotefamily]{SMOTE}},
#'   \code{\link[smotefamily]{Borderline-SMOTE}},
#'   \code{\link[smotefamily]{DBSMOTE}}, \code{\link[smotefamily]{SLS}},
#'   \code{\link[smotefamily]{RSLS}}
#' @param filtering Logical (TRUE or FALSE) indicating wheter to apply filtering
#'   of oversampled instances with \code{\link{NEATER}} algorithm.
#' @param classAttr String. Indicates the class attribute from \code{dataset}.
#'   Must exist in it.
#'
#' @return A balanced \code{data.frame} with same structure as \code{dataset},
#'   containing both original instances and new ones
#' @export
#'
#' @examples
resample <- function(dataset, ratio = NA, method = c("racog", "wracog",
                     "pdfos", "rwo", "mwmote", "ADASYN", "ANS", "SMOTE",
                     "Borderline-SMOTE", "DBSMOTE", "SLS", "RSLS"),
                     filtering = FALSE, classAttr = "Class",
                     wrapper = NA, ...){
  checkDataset(dataset, "dataset")
  checkDatasetClass(dataset, classAttr, "dataset")
  originalShape <- datasetStructure(dataset, classAttr)

  minority <- selectMinority(dataset, classAttr)
  minoritySize <- nrow(minority)
  majoritySize <- nrow(dataset) - minoritySize
  currentRatio <- minoritySize / majoritySize
  dupSize <- ceiling(ratio / currentRatio - 1)
  classIndex <- which(names(dataset) == classAttr)

  if(method %in% c("racog", "pdfos", "rwo", "mwmote")){
    # Checks
    if(is.na(ratio))
      stop("ratio cannot be undefined for the selected method")
    if(ratio <= 0 || ratio > 1)
      stop("ratio must be a real number between 0 and 1")

    if(ratio < currentRatio)
      stop("ratio must be greater than current ratio of imbalance to perform
          an oversampling")

    # Compute number of required synthetic positive instances
    numInstances <- majoritySize * ratio - minoritySize

    # Evaluate method
    selectedMethod <- eval(as.name(method))
    newSamples <- selectedMethod(dataset, numInstances, classAttr)
  } else if(method == "wracog"){
      if(is.na(wrapper))
        stop("wrapper argument not found")
      if(! wrapper %in% c("C5.0", "Knn"))
        stop("Possible values when using wracog are: C5.0, knn")

      if(wrapper == "C5.0"){
        myWrapper <- structure(list(), class="C50Wrapper")
        trainWrapper.C50Wrapper <- function(wrapper, train, trainClass){
          C50::C5.0(train, trainClass, ...)
        }
      } else{
        if(wrapper == "Knn"){
          myWrapper <- structure(list(), class = "KNNWrapper")
          predict.KNN <- function(model, test){
            FNN::knn(model$train, test, model$trainClass, ...)
          }
          trainWrapper.KNNWrapper <- function(wrapper, train, trainClass){
            myKNN <- structure(list(), class = "KNN")
            myKNN$train <- train
            myKNN$trainClass <- trainClass
            myKNN
          }
        }
      }
      trainfold <- sample(1:nrow(dataset), nrow(dataset)/2, FALSE)
      newSamples <- wracog(dataset[trainFold, ], dataset[-trainFold, ],
                           myWrapper, classAttr)
  } else if(method == "ADASYN"){
    newSamples <- smotefamily::ADAS(dataset[, -classIndex],
                                    dataset[, classIndex])
    newSamples <- newSamples$syn_data
    newSamples <- newSamples[, -ncol(newSamples)]
    newSamples <- normalizeNewSamples(originalShape, newSamples)
  } else if(method == "ANS"){
    newSamples <- smotefamily::ANS(dataset[, -classIndex],
                                   dataset[, classIndex],
                                   dupSize)
    newSamples <- newSamples$syn_data
    newSamples <- newSamples[, -ncol(newSamples)]
    newSamples <- normalizeNewSamples(originalShape, newSamples)
  } else if(method == "smotefamily"){
    newSamples <- smotefamily::ANS(dataset[, -classIndex],
                                   dataset[, classIndex],
                                   dupSize)
    newSamples <- newSamples$syn_data
    newSamples <- newSamples[, -ncol(newSamples)]
    newSamples <- normalizeNewSamples(originalShape, newSamples)
  }

  if(filtering)
    newSamples <- neater(dataset, newSamples)

  result <- rbind(dataset, newSamples)
  result
}
