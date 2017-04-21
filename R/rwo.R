
rwo <- function(dataset, num.instances){
  classes <- unique(dataset$class)
  classes.counts <- sapply(classes, function(c){ length(which(dataset$class==c)) })
  minority.class <- classes[ which.min(classes.counts) ]

  minority <- dataset[dataset$class == minority.class, ]

  n <- nrow(dataset)
  n.sqrt <- sqrt(n)


  new.samples <- apply(minority, MARGIN=2, function(col){
    # If attribute is numeric, generate new minority sample preserving mean and variance of existent samples
    if(is.numeric(col)){
      variance <- var(col)
      col <- col[minority.indexes]

      col - variance/n.sqrt * runif(length(col) * num.instances)

    # Else if attribute is not numeric, make a roulette out of values for the attribute
    } else{
      dist <- table(col)
      dist.values <- names(dist)
      dist.probs <- unname(dist)

      result <- sample(dist.values, length(col) * num.instances, replace=T, prob = dist.probs)
    }
  })

  new.samples
}
