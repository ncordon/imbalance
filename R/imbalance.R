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
#'   \code{\link{pdfos}}, \code{\link{mwmote}}, \code{\link[smotefamily]{ADAS}},
#'   \code{\link[smotefamily]{ANS}}, \code{\link[smotefamily]{SMOTE}},
#'   \code{\link[smotefamily]{BLSMOTE}}, \code{\link[smotefamily]{DBSMOTE}},
#'   \code{\link[smotefamily]{SLS}}, \code{\link[smotefamily]{RSLS}}
#' @param filtering Logical (TRUE or FALSE) indicating wheter to apply filtering
#'   of oversampled instances with \code{\link{neater}} algorithm.
#' @param classAttr String. Indicates the class attribute from \code{dataset}.
#'   Must exist in it.
#' @param wrapper A \code{character} corresponding to wrapper to apply if
#'   selected method is \code{\link{wracog}}. Possibilities are: \code{C5.0} and
#'   \code{KNN}.
#' @param ... Further arguments to apply in selected method
#'
#' @return A balanced \code{data.frame} with same structure as \code{dataset},
#'   containing both original instances and new ones
#' @export
#'
#' @examples
#' data(glass0)
#'
#' # Oversample glass0 to get an imbalance ratio of 0.8
#' newDataset <- oversample(glass0, ratio = 0.8, method = "MWMOTE")
#'
oversample <- function(dataset, ratio = NA, method = c("RACOG", "wRACOG",
                      "PDFOS", "RWO", "ADASYN", "adaptative", "SMOTE",
                      "MWMOTE", "borderline-SMOTE", "density-SMOTE",
                      "SLMOTE", "relocating-SMOTE"),
                      filtering = FALSE, classAttr = "Class",
                      wrapper = NA, ...){
  checkDataset(dataset, "dataset")
  checkDatasetClass(dataset, classAttr, "dataset")
  originalShape <- datasetStructure(dataset, classAttr)

  minority <- selectMinority(dataset, classAttr)
  minoritySize <- nrow(minority)
  majoritySize <- nrow(dataset) - minoritySize
  currentRatio <- minoritySize / majoritySize
  classIndex <- which(names(dataset) == classAttr)
  method <- match.arg(method)

  # Compute number of required synthetic positive instances
  numInstances <- ceiling(majoritySize * ratio - minoritySize)
  dupSize <- 0 #ceiling(numInstances / minoritySize)

  method <- switch(method,
                   "RACOG"  = "racog",
                   "wRACOG" = "wracog",
                   "PDFOS"  = "pdfos",
                   "RWO"    = "rwo",
                   "ADASYN" = "ADAS",
                   "adaptative" = "ANS",
                   "SMOTE"  = "SMOTE",
                   "MWMOTE" = "mwmote",
                   "borderline-SMOTE" = "BLSMOTE",
                   "density-SMOTE" = "DBSMOTE",
                   "SLMOTE" = "SLS",
                   "relocating-SMOTE" = "RSLS")

  # ratio parameter is mandatory when ADASYN is not picked
  if(! method %in% c("ADAS", "wracog")){
    # Checks
    if(is.na(ratio))
      stop("ratio cannot be undefined for the selected method")
    if(ratio <= 0 || ratio > 1)
      stop("ratio must be a real number between 0 and 1")

    if(ratio < currentRatio)
      stop("ratio must be greater than current ratio of imbalance to perform
            an oversampling")
  }


  if(method %in% c("racog", "pdfos", "rwo", "mwmote")){
    # Evaluate method
    selectedMethod <- eval(as.name(method))
    newSamples <- selectedMethod(dataset = dataset,
                                 numInstances = numInstances,
                                 classAttr = classAttr)
  } else if(method == "wracog"){
      if(is.na(wrapper))
        stop("wrapper argument not found")
      if(! wrapper %in% c("C5.0", "KNN"))
        stop("Possible values when using wracog are: C5.0, KNN")

      if(wrapper == "C5.0"){
        myWrapper <- structure(list(), class="C50Wrapper")
        trainWrapper.C50Wrapper <<- function(wrapper, train, trainClass){
          C50::C5.0(train, trainClass, ...)
        }
      } else{
        if(wrapper == "KNN"){
          myWrapper <- structure(list(), class = "KNNWrapper")
          predict.KNN <- function(model, test){
            FNN::knn(model$train, test, model$trainClass, ...)
          }
          trainWrapper.KNNWrapper <<- function(wrapper, train, trainClass){
            myKNN <- structure(list(), class = "KNN")
            myKNN$train <- train
            myKNN$trainClass <- trainClass
            myKNN
          }
        }
      }
      trainFold <- sample(1:nrow(dataset), size = nrow(dataset)/2, replace = FALSE)
      newSamples <- wracog(dataset[trainFold, ], dataset[-trainFold, ],
                           wrapper = myWrapper, classAttr = classAttr)
  } else{
    selectedMethod <- eval(parse(text = paste("smotefamily::", method, sep = "")))

    if(method == "ADAS"){
      newSamples <- selectedMethod(X = dataset[, -classIndex],
                                   target = dataset[, classIndex],
                                   ...)
    } else if(method == "SMOTE"){
      newSamples <- selectedMethod(X = dataset[, -classIndex],
                                   target = dataset[, classIndex],
                                   dup_size = dupSize, ...)

    } else{
      newSamples <- selectedMethod(X = dataset[, -classIndex],
                                   target = dataset[, classIndex],
                                   dupSize = dupSize, ...)
    }

    newSamples <- newSamples$syn_data

    if(!is.na(ratio)){
      if(numInstances <= nrow(newSamples) || method != "ADAS")
      newSamples <- newSamples[sample(1:nrow(newSamples),
                                      size = numInstances, replace = FALSE ), ]
      else
        warning("given ratio not achievable with ADASYN")
    }

    newSamples <- newSamples[, -ncol(newSamples)]
    newSamples <- normalizeNewSamples(originalShape, newSamples)
  }

  if(filtering)
    newSamples <- neater(dataset, newSamples)

  result <- rbind(dataset, newSamples)
  result
}
