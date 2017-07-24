#' Rapidy Converging Gibbs algorithm
#'
#' Allows you to treat imabalanced datasets and \code{racog} generate synthetic
#' minority examples approximating their probability distribution
#'
#' Aproximates minority distribution using Gibbs Sampler. Dataset must be
#' discretized and numeric. In each iteration, it builds a new sample using a
#' Markov chain. It discards first \code{burnin} iterations, and from then on,
#' it validates each \code{lag} example as a new minority example. It generates
#' \code{d(iterations-burnin)/lag} where \code{d} is minority examples. number.
#'
#' @param dataset A \code{{data.frame}} to treat.
#' @param burnin Integer. It determines how many examples generated for a given
#'   one are going to be discarded firstly.
#' @param lag Integer. Number of iterations between new generated example for a
#'   minority one.
#' @param iterations Integer. Number of iterations to run for each minority
#'   example.
#' @param classAttr String. Indicates the class attribute from \code{dataset}.
#'   Must exsits in it.
#' @param minorityClass Indicates the minority class value. If not present,
#'   \code{racog} will calculate it authomatically.
#'
#' @return new samples, a \code{data.frame} with the same structure as
#'   \code{dataset} of the synthetic examples generated
#'
#' @examples
#' # Makes a dataset imbalanced
#' iris <- iris[1:125, ]
#' racog(iris, iterations = 1000, classAttr = "Species")
#'
#' # Generates 25 new examples of class 'virginica'
#' newSamples <- racog(iris, burnin = 95, iterations = 100,
#'                    classAttr = "Species", minorityClass = "virginica")
#'
racog <- function(dataset, burnin = 10, lag = 10, iterations,
                  classAttr = "class", minorityClass){
  if(! classAttr %in% names(dataset))
    stop("class attribute not found in dataset. Please provide a valid class attribute")
  if(missing(minorityClass))
    minorityClass <- whichMinorityClass(dataset, classAttr)
  else if(! minorityClass %in% levels(dataset[, classAttr]))
    stop("Minority class not found in dataset")


  minority <- dataset[dataset[, classAttr] == minorityClass, ]
  attrs <- names(minority)
  attrs <- attrs[attrs != classAttr]
  minority <- minority[, attrs]

  DT <- bnlearn::chow.liu(minority)$arcs
  # Choose only one direction for the arcs (the odd ones) and make tree directed
  tree <- unname(DT[ seq(1, nrow(DT), 2), ])
  makeDirected(tree)

  # Calculate conditioned probability distributions
  # Cols are variables to which we are conditioning to
  condProbs <- apply(tree, MARGIN = 1, function(r){
    table(minority[, r[2]], minority[, r[1]])
  })

  # Calculate absolute probability distributions
  absoluteProbs <- apply(minority, MARGIN = 2, function(col){
    table(col)
  })
  absoluteProbs <- lapply(absoluteProbs, function(x){ x/sum(x) })


  newSamples <- list()

  # For each minority example, create (iterations - burnin)/lag
  # new examples, approximating minority distribution with a Gibss sampler
  for(i in 1:nrow(minority)){
    x <- minority[i,]
    for(t in 1:iterations){
      for(attr in attrs){
        # Calc vars that are being conditioned to attr
        conditioned <- which(tree[,1] == attr)
        # Calc var that attr is conditioned to
        conditioning <- which(tree[,2] == attr)

        first <- sapply(conditioned, function(k){
          r <- condProbs[[k]][toString(x[, tree[,2][k]]), ]
          r/sum(r) * absoluteProbs[[attr]]
        })

        second <- sapply(conditioning, function(k){
          r <- condProbs[[k]][, toString(x[, tree[,1][k]])]
          r/sum(r)
        })

        probVectors <- cbind(first, second)

        # Prob of attr. is product of probabilites from the dependence tree
        ithProb <- apply(probVectors, MARGIN = 1, function(r){
          prod(unlist(r))
        })

        # If all probabilities are zero, create vector with same probabilities
        if(! any(ithProb != 0))
          ithProb <- rep(1,length(ithProb))

        x[, attr] = sample( row.names(probVectors), 1, prob = ithProb )
      }

      if(t > burnin && t%%lag == 0){
        newSamples[[length(newSamples)+1]] <- x
      }
    }
  }

  # Output
  newSamples <- do.call(rbind, newSamples)
  newSamples[, classAttr] <- factor(minorityClass, levels = levels(dataset[, classAttr]))
  rownames(newSamples) <- c()
  newSamples
}
