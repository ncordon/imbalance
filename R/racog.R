# dataset <- iris
# class.attr <- "Species"
# lag <- 10
# iterations <- 1000
# burn.in.period <- 50


racog <- function(dataset, burn.in.period, lag, iterations, class.attr = "class", minority.class){
  minority.class <- which.minority(dataset, class.attr)
  minority <- dataset[dataset[, class.attr] == minority.class, ]
  #minority <- as.data.frame( apply(minority, MARGIN=2, factor) )
  attrs <- names(minority)
  attrs <- attrs[attrs != class.attr]
  minority <- minority[, attrs]

  DT <- bnlearn::chow.liu(minority)$arcs
  # Choose only one direction for the arcs (the even ones)
  tree <- unname(DT[ seq(1, nrow(DT), 2), ])

  # Reverse arcs in tree if not adirected tree
  if(length(unique(tree[,2])) < nrow(tree)){
    tree <- tree[,c(2,1)]
  }

  # Calculate prob distributions
  # Cols are variables to which we are conditioning to
  probs <- apply(tree, MARGIN = 1, function(x){
    table(minority[,x[2]], minotiry[,x[1]])
  })

  #table(dataset[,tree[1,2]], dataset[,tree[1,1]])

  new.samples <- list()

  for(i in 1:nrow(minority)){
    x <- minority[i,]
    for(t in 1:iterations){
      for(attr in attrs){
        # Calc vars that are being conditioned to attr
        from <- which(tree[,1] == attr)
        # Calc var that attr is conditioned to
        to <- which(tree[,2] == attr)

        first <- sapply(from, function(k){
          r <- probs[[k]][ x[, tree[,2][k]], ]
          prob.attr <- as.vector(table(minority[, attr]))
          r/sum(r) * prob.attr/sum(prob.attr)
        })

        second <- sapply(to, function(k){
          r <- probs[[k]][ ,x[, tree[,1][k]]]
          r/sum(r)
        })

        prob.vectors <- cbind(first, second)

        # Prob of attr. is product of probabilites from the dependence tree
        ith.prob <- apply(prob.vectors, MARGIN = 1, function(r){
          prod(unlist(r))
        })

        # If all probabilities are zero, create vector with same probabilities
        if(! any(ith.prob != 0))
          ith.prob <- rep(1,length(ith.prob))

        x[, attr] = sample( row.names(prob.vectors), 1, prob = ith.prob )
      }

      if(t > burn.in.period && t%%lag == 0){
        new.samples[[length(new.samples)+1]] <- x
      }
    }
  }

  # Output
  new.samples
}
