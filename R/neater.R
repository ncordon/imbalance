# dataset and new.samples must have the same class name attribute appended
neater <- function(dataset, new.samples, k, n.iter, class.attr){
  n.old <- nrow(dataset)
  minority.class <- new.samples[1, class.attr]
  classes <- dataset[, class.attr]
  minority.indexes <- which(classes != class.attr)

  dataset <- rbind(dataset, new.samples)
  knn.indexes <- FNN::knnx.index(dataset, new.samples, k = k)
  # Matrix of probabilities of belonging to each class, first -> minority, second -> not minority


  probs <- matrix(nrow = nrow(dataset), ncol = 2)
  probs[ minority.indexes, 1] <- 1
  probs[ minority.indexes, 2] <- 0
  probs[-minority.indexes, 1] <- 0
  probs[-minority.indexes, 2] <- 1
  probs[(n.old + 1):nrow(dataset), ] <- 0.5


  # List with the payoffs for each synthetic sample respect to its k-nearest neighbours
  partial.payoffs <- lapply(1:nrow(new.samples), function(index){
    sapply(knn.indexes[index, ], function(j){
      diff <- dataset[j, ] - dataset[index, ]
      1 / sum(diff * diff)
    })
  })


  lapply(1:nrow(new.samples), function(index){
    # Calculate payoff for index tuple
    sum( sapply(knn.indexes[index, ], function(j){
      partial.payoffs[[i]][j] * as.numeric(probs[i] * probs[j])
    }) )
  })

}
