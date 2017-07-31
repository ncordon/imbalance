#' Rapidy Converging Gibbs algorithm.
#'
#' Allows you to treat imabalanced datasets and \code{racog} generate synthetic
#' minority examples approximating their probability distribution.
#'
#' Aproximates minority distribution using Gibbs Sampler. Dataset must be
#' discretized and numeric. In each iteration, it builds a new sample using a
#' Markov chain. It discards first \code{burnin} iterations, and from then on,
#' it validates each \code{lag} example as a new minority example. It generates
#' \eqn{d \frac{iterations-burnin}{lag}} where \eqn{d} is minority examples number.
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
#'
#' # Generates new minority examples
# newSamples <- racog(iris, burnin = 95, iterations = 100,
#                    classAttr = "Species")
#'
racog <- function(dataset, burnin = 100, lag = 20, iterations, classAttr = "class"){
  if(!is.data.frame(dataset))
    stop("dataset must be a data.frame")
  if(! classAttr %in% names(dataset))
    stop("class attribute not found in dataset")
  if(!is.numeric(burnin) || !is.numeric(lag) || !is.numeric(iterations) ||
     burnin < 0 || lag < 0 || iterations < 0)
    stop("burnin, lag and iterations must be positive integers")


  # Calcs minority class and instances
  minorityClass <- whichMinorityClass(dataset, classAttr)
  minority <- dataset[dataset[, classAttr] == minorityClass,
                      names(dataset) != classAttr]

  gibbsSampler <- .makeGibbsSampler(minority)
  newSamples <- list()

  # For each minority example, create (iterations - burnin)/lag
  # new examples, approximating minority distribution with a Gibss sampler
  for(i in 1:nrow(minority)){
    x <- minority[i,]
    for(t in 1:iterations){
      # Generate new sample using Gibbs Sampler
      x <- gibbsSampler(x)

      if(t > burnin && t%%lag == 0){
        newSamples[[length(newSamples) + 1]] <- x
      }
    }
  }

  # Output
  newSamples <- do.call(rbind, newSamples)
  newSamples[, classAttr] <- minorityClass
  rownames(newSamples) <- c()
  newSamples
}



#' Wrapper for Rapidy Converging Gibbs algorithm.
#'
#' @param train
#' @param validation
#' @param wrapper
#' @param classAttr
#' @param minorityClass
#' @param slideWin
#' @param threshold
#'
#' @return
#' @export
#'
#' @examples
#' trainWrapper <- function(wrapper, train, trainClass){ UseMethod("trainWrapper") }
#' mywrapper <- structure(list(), class="C50Wrapper")
#' trainWrapper.C50Wrapper <- function(wrapper, train, trainClass){
#'   C50::C5.0(train, trainClass)
#' }
#'
#' wracog(iris[1:100, ], iris[100:150, ], mywrapper, "Species")
#'
wracog <- function(train, validation, wrapper, classAttr = "class",
                   slideWin = 10, threshold = 0.02){
  if(!is.data.frame(train) || !is.data.frame(validation) ||
     names(train) != names(validation))
    stop("train and validation must be data.frames with the same colnames")
  if(! classAttr %in% names(train))
    stop("class attribute not found in dataset")
  if((!is.numeric(slideWin) || !is.numeric(threshold)) ||
     slideWin < 0 || threshold < 0 || threshold > 1)
    stop("slideWin must be a positive integer \n  threshold must be in ]0,1[")

  # Calcs minority class
  minorityClass <- whichMinorityClass(train, classAttr)

  # Strip class column from both train and validation
  minority <- train[train[, classAttr] == minorityClass,
                    names(train) != classAttr]
  trainClass <- train[, classAttr]
  validationClass <- validation[, classAttr]
  train <- train[, names(train) != classAttr]
  validation <- validation[, names(validation) != classAttr]

  # Wrapper for the Gibbs sampler with input and outpus as data.frame
  gibbsSampler <- .makeGibbsSampler(minority)
  dfGibbsSampler <- function(samples){
    samples <- split(samples, seq(nrow(samples)))
    do.call(rbind.data.frame, lapply(samples, gibbsSampler))
  }

  # Value for lasts winSlides standard deviations
  lastSlides <- rep(Inf, slideWin)

  model <- trainWrapper(wrapper, train, trainClass)
  newSamples <- data.frame()

  while(.naReplace(sd(lastSlides), Inf) >= threshold){
    minority <- dfGibbsSampler(minority)
    prediction <- predict(model, minority)
    misclassified <- minority[prediction != minorityClass, ]
    newSamples <- rbind.data.frame(newSamples, minority)
    train <- rbind.data.frame(train, misclassified)
    trainClass <- .appendfactor(trainClass, rep(minorityClass, nrow(misclassified)))
    model <- trainWrapper(wrapper, train, trainClass)
    prediction <- predict(model, validation)

    # Measure of the quality of the newTrain
    qMeasure <- .sensitivity(prediction, validationClass)
    lastSlides <- c(qMeasure, lastSlides)
    lastSlides <- lastSlides[1:slideWin]
  }

  newSamples[, classAttr] <- minorityClass
  rownames(newSamples) <- c()
  newSamples
}



#' Make tree directed.
#'
#' Returns a directed tree build up from an undirected one, complying with the
#' condition that each non-root node has a single parent.
#'
#' @param tree Matrix nx2 columns denoting undirected arcs of a tree.
#' @return Matrix of directed arcs. Arcs are directed from the first coordinate
#'   towards second.
#' @keywords internal
#'
#' @examples
#' DT <- bnlearn::chow.liu(iris[, names(iris) != "Species"])$arcs
#'
#' tree <- unname(DT[ seq(1, nrow(DT), 2), ])
#' makeDirected(tree)
#'
.makeDirected <- function(tree){
  visited <- c()
  # For each arc, marks the second node as visited
  # If for a node second coordinate has already been visited, it inverts the sense
  # of the arc
  for (k in 1:nrow(tree)){
    if(tree[k,2] %in% visited){
      tree[k,] <- tree[k,c(2,1)]
    }
    visited <- c(visited, tree[k,2])
  }

  tree
}


#' Generate Gibss Sampler
#'
#' Generates Gibbs Sampler algorithm for approximate samples distribution
#' @param samples A numerical dataframe of samples whose distribution
#'   approximate
#'
#' @return GibbsSampler. A function that receives a sample, and generates a new
#'   one using the distribution
#' @keywords internal
.makeGibbsSampler <- function(samples){
  attrs <- names(samples)
  DT <- bnlearn::chow.liu(samples)$arcs
  # Choose only one sense for the arcs (the odd ones) and make tree directed
  tree <- unname(DT[ seq(1, nrow(DT), 2), ])
  tree <- .makeDirected(tree)

  # Calculate conditioned probability distributions
  # Cols are variables to which we are conditioning to
  condProbs <- apply(tree, MARGIN = 1, function(r){
    table(samples[, r[2]], samples[, r[1]])
  })

  # Calculate absolute probability distributions
  absoluteProbs <- apply(samples, MARGIN = 2, function(col){
    table(col)
  })

  absoluteProbs <- lapply(absoluteProbs, function(x){ x/sum(x) })

  dependences <- lapply(attrs, function(attr){
    conditioned <- which(tree[,1] == attr)
    # Calc var that attr is conditioned to
    conditioning <- which(tree[,2] == attr)
    if(length(conditioned) > 0)
      values <- colnames(condProbs[[conditioned[1]]])
    else
      values <- rownames(condProbs[[conditioning[1]]])

    list(conditioned, conditioning, values)
  })

  # Generate the Gibbs Sampler
  gibbsSampler <- function(x){
    for(k in 1:length(dependences)){
      attr <- attrs[k]

      # Calc vars that are being conditioned to attr
      conditioned <- dependences[[k]][[1]]
      # Calc var that attr is conditioned to
      conditioning <- dependences[[k]][[2]]

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

      x[, attr] = sample( dependences[[k]][[3]], 1, prob = ithProb )
    }
    # Return new sample
    x
  }

  # Return Gibbs Sampler
  gibbsSampler
}
