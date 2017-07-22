which.minority <- function(dataset, class.attr){
  classes <- dataset[, class.attr]
  counts <- table(classes)
  names(which.min(counts))
}


make.directed <- function(tree){
  visited <- c()
  for (k in 1:nrow(tree)){
    if(tree[k,2] %in% visited){
      tree[k,] <- tree[k,c(2,1)]
    }
    visited <- c(visited, tree[k,2])
  }

  tree
}


balance.dataset <- function(dataset, ratio, method = c("pdfos", "racog"), class.attr = "class"){
  # Check of arguments
  if (missing(dataset))
    stop("dataset must not be empty")
  if (missing(ratio) || !is.numeric(ratio) || ratio < 1)
    stop("ratio must be a number greater or equal than 1")
  if (!class.attr %in% names(dataset))
    stop(paste("Class attribute '", class.attr, "' not found in dataset", sep = ""))


  classes <- unique(dataset[, class.attr])
  classes.counts <- sapply(classes, function(c){ length(which(dataset[, class.attr] == c)) })
  minority.class <- classes[which.min(classes.counts)]

  minority <- dataset[dataset[, class.attr] == minority.class, ]
  minority.size <- nrow(minority)

  # Delete class attribute
  minority <- minority[, names(minority) != class.attr]


  if (method == "pdfos"){
    n.instances <- minority.size * ratio
    new.samples <- pdfos(minority, n.instances)
    names(new.samples) <- names(minority)
    # Add minority class attribute and bind new instances
    new.samples[ class.attr ] <- minority.class
    result <- rbind(dataset, new.samples)
    rownames(result) <- 1:nrow(result)
  }

  result
}
