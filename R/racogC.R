#' Rapidy Converging Gibbs algorithm.
#'
#' Allows you to treat imabalanced discrete numeric datasets by generating
#' synthetic minority examples by approximating their probability distribution.
#'
#' Aproximates minority distribution using Gibbs Sampler. Dataset must be
#' discretized and numeric. In each iteration, it builds a new sample using a
#' Markov chain. It discards first \code{burnin} iterations, and from then on,
#' it validates each \code{lag} example as a new minority example. It generates
#' \eqn{d (iterations-burnin)/lag} where \eqn{d} is minority examples number.
#'
#' @param dataset data.frame to treat. All columns, except \code{classAttr} one,
#'   have to be numeric.
#' @param burnin Integer. It determines how many examples generated for a given
#'   one are going to be discarded firstly. By default, 100.
#' @param lag Integer. Number of iterations between new generated example for a
#'   minority one. By default, 20.
#' @param iterations Integer. Number of iterations to run for each minority
#'   example.
#' @param classAttr String. Indicates the class attribute from \code{dataset}.
#'   Must exist in it.
#'
#' @return A \code{data.frame} with the same structure as \code{dataset},
#'   containing the synthetic examples generated.
#' @examples
#' data(iris0)
#' set.seed(12345)
#'
#' # Generates new minority examples
#' newSamples <- racog(iris0, burnin = 10, iterations = 100, classAttr = "Class")
#'
racogC <- function(dataset, burnin = 100, lag = 20, iterations = 100, classAttr = "Class"){
  if(!is.data.frame(dataset))
    stop("dataset must be a data.frame")
  if(!classAttr %in% names(dataset))
    stop(paste(classAttr, "attribute not found in dataset"))
  if(any(! .colTypes(dataset, exclude = classAttr) == "numeric"))
    stop("all columns of dataset must be numeric")
  if(!is.numeric(burnin) || !is.numeric(lag) || !is.numeric(iterations) ||
     burnin < 0 || lag < 0 || iterations < 0)
    stop("burnin, lag and iterations must be positive integers")


  # Calcs minority class and instances
  minorityClass <- .whichMinorityClass(dataset, classAttr)
  minority <- dataset[dataset[, classAttr] == minorityClass,
                      names(dataset) != classAttr]

  attrs <- names(minority)
  DT <- bnlearn::chow.liu(minority)$arcs
  # Choose only one sense for the arcs (the odd ones) and make tree directed
  edges <- unname(DT[ seq(1, nrow(DT), 2), ])
  edges <- .makeDirected(edges)
  # Convert names to indexes in C style
  edges <- apply(edges, MARGIN = c(1,2), function(attr){ which(attrs == attr) - 1})

  root = attrs[! attrs %in% edges[, 2]]
  root = which(attrs = attr) - 1

  gibbsSampler <- .makeGibbsSampler(minority)
  newSamples <- data.frame(matrix(ncol = ncol(minority), nrow = 0))

  # For each minority example, create (iterations - burnin)/lag
  # new examples, approximating minority distribution with a Gibss sampler
  # for(k in seq_len(iterations)){
  #   # Generate new sample using Gibbs Sampler
  #   minority <- data.frame(t(apply(minority, MARGIN = 1, gibbsSampler)))
  #
  #   if(k > burnin && k%%lag == 0)
  #     newSamples <- rbind.data.frame(newSamples, minority)
  # }

  racog_(minority, edges, root, iterations, burnin, lag)


  # Prepare newSamples output
  #.normalizeNewSamples(newSamples, minorityClass, names(minority), classAttr)
}
