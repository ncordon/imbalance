# dataset and new.samples must have the same class name attribute appended
# new.samples <- smotefamily::SMOTE(iris[,-5], iris[,5])$syn_data
# dataset <- iris
# new.samples$Species <- new.samples$class
# new.samples <- new.samples[, names(new.samples)!="class"]

neater <- function(dataset, new.samples, class.attr, k, n.iter, smooth.parameter){
  n.old <- nrow(dataset)
  minority.class <- new.samples[1, class.attr]
  classes <- dataset[, class.attr]
  minority.indexes <- which(classes == minority.class)

  dataset <- rbind(dataset, new.samples)
  dataset <- dataset[, names(dataset) != class.attr]
  new.samples <- new.samples[, names(new.samples) != class.attr]
  knn.indexes <- FNN::knnx.index(dataset, new.samples, k = k)
  # Matrix of probabilities of belonging to each class, first -> minority, second -> not minority

  probs <- matrix(nrow = nrow(dataset), ncol = 2)
  probs[ minority.indexes, 1] <- 1
  probs[ minority.indexes, 2] <- 0
  probs[-minority.indexes, 1] <- 0
  probs[-minority.indexes, 2] <- 1
  probs[(n.old + 1):nrow(dataset), ] <- 0.5

  # List with the payoffs for each synthetic sample respect to its k-nearest neighbours
  partial.payoffs <- lapply(1:nrow(new.samples), function(i){
    sapply(knn.indexes[i, ], function(j){
      diff <- dataset[j, ] - dataset[i, ]
      1 / (sum(diff * diff) + 1)
    })
  })

  for(i in 1:n.iter){
    payoffs <- sapply(1:nrow(new.samples), function(i){
      # Calculate payoff for ith tuple
      sum( partial.payoffs[[i]] * (probs[knn.indexes[i, ], ] %*% probs[n.old + i, ]) )
    })

    avg.payoff <- mean(payoffs)

    updated.probs <- lapply(1:length(payoffs), function(i){
      min.prob <- probs[n.old + i,1] * (smooth.parameter + payoffs[i]) / (smooth.parameter + avg.payoff)
      min.prob <- min(1, min.prob)
      c(min.prob, 1-min.prob)
    })

    # Updates probabilities of each class
    updated.probs <- do.call(rbind, updated.probs)
    probs <- rbind(probs[1:n.old, ], updated.probs)
  }

  # Select synthetic instances whose probabily of belonging to the minority class is greater than a half
  good.syn.samples <-  which(probs[(n.old+1):nrow(probs), 1] > 0.5)
  new.samples <- new.samples[good.syn.samples, ]
  # Append class column to minority samples and return them
  new.samples[, class.attr] <- minority.class

  new.samples
}
