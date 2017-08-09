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

#' Wrapper to ease the class balancing preprocessing task for binary class
#' \code{dataset}
#'
#' @param dataset A binary class \code{data.frame} to balance.
#' @param ratio Numeric greater than 1.
#' @param method A \code{character} corresponding to method to apply. Possible
#'   methods are: \code{\link{racog}}, \code{\link{wracog}}, \code{\link{rwo}},
#'   \code{\link{pdfos}}.
#' @param filter Logical wheter to apply filtering of oversampled instances.
#' @param classAttr String. Indicates the class attribute from \code{dataset}.
#'   Must exist in it.
#'
#' @return \code{data.frame} with balanced classes.
#' @export
#'
#' @examples
balanceDataset <- function(dataset, ratio, method, filter = FALSE,
                           classAttr = "class"){
  if(!is.data.frame(dataset))
    stop("dataset must be a data.frame")
  if (!classAttr %in% names(dataset))
    stop(paste(classAttr, "attribute not found in dataset"))


  classes <- unique(dataset[, classAttr])
  classes.counts <- sapply(classes, function(c){
    length(which(dataset[, classAttr] == c))
  })
  minority.class <- classes[which.min(classes.counts)]

  minority <- dataset[dataset[, classAttr] == minority.class, ]
  minority.size <- nrow(minority)

  # Delete class attribute
  minority <- minority[, names(minority) != classAttr]


  if (method == "pdfos"){
    n.instances <- minority.size * ratio
    new.samples <- pdfos(minority, n.instances)
    names(new.samples) <- names(minority)
    # Add minority class attribute and bind new instances
    new.samples[ classAttr ] <- minority.class
    result <- rbind(dataset, new.samples)
    rownames(result) <- 1:nrow(result)
  }

  result
}
