#' Fiter of oversampled data based on non-cooperative game theory
#'
#' @param dataset
#' @param newSamples
#' @param k
#' @param numIterations
#' @param smooth
#' @param classAttr
#'
#' @return
#' @export
#'
#' @examples
#' # dataset and newSamples must have the same class name attribute appended
#' newSamples <- smotefamily::SMOTE(iris[,-5], iris[,5])$syn_data
#' dataset <- iris
#' newSamples$Species <- newSamples$class
#' newSamples <- newSamples[, names(newSamples)!="class"]
neater <- function(dataset, newSamples, k, numIterations, smooth,
                   classAttr = "Class"){
  oldSize <- nrow(dataset)
  minorityClass <- newSamples[1, classAttr]
  classes <- dataset[, classAttr]
  minorityIndexes <- which(classes == minority.class)

  dataset <- rbind(dataset, newSamples)
  dataset <- dataset[, names(dataset) != classAttr]
  newSamples <- newSamples[, names(newSamples) != classAttr]
  knnIndexes <- FNN::knnx.index(dataset, newSamples, k = k)
  # Matrix of probabilities of belonging to each class, first -> minority, second -> not minority

  probs <- matrix(nrow = nrow(dataset), ncol = 2)
  probs[ minorityIndexes, 1] <- 1
  probs[ minorityIndexes, 2] <- 0
  probs[-minorityIndexes, 1] <- 0
  probs[-minorityIndexes, 2] <- 1
  probs[(oldSize + 1):nrow(dataset), ] <- 0.5

  # List with the payoffs for each synthetic sample respect to its k-nearest neighbours
  partial.payoffs <- lapply(1:nrow(newSamples), function(i){
    sapply(knnIndexes[i, ], function(j){
      diff <- dataset[j, ] - dataset[i, ]
      1 / (sum(diff * diff) + 1)
    })
  })

  for(i in 1:numIterations){
    payoffs <- sapply(1:nrow(newSamples), function(i){
      # Calculate payoff for ith tuple
      sum( partial.payoffs[[i]] * (probs[knnIndexes[i, ], ] %*% probs[oldSize + i, ]) )
    })

    avgPayoff <- mean(payoffs)

    updatedProbs <- lapply(1:length(payoffs), function(i){
      minProb <- probs[oldSize + i,1] * (smooth + payoffs[i]) / (smooth + avgPayoff)
      minProb <- min(1, minProb)
      c(minProb, 1-minProb)
    })

    # Updates probabilities of each class
    updatedProbs <- do.call(rbind, updatedProbs)
    probs <- rbind(probs[1:oldSize, ], updatedProbs)
  }

  # Select synthetic instances whose probabily of belonging to the minority class is greater than a half
  goodSamples <-  which(probs[(oldSize+1):nrow(probs), 1] > 0.5)
  newSamples <- newSamples[goodSamples, ]
  # Append class column to minority samples and return them
  newSamples[, classAttr] <- minority.class

  newSamples
}
