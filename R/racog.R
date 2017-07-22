# dataset <- iris
# class.attr <- "Species"
# lag <- 10
# iterations <- 1000
# burn.in.period <- 50
# class.attr = "Species"

racog <- function(dataset, burn.in.period, lag, iterations, class.attr = "class", minority.class){
  if(! class.attr %in% names(dataset))
    stop("class attribute not found in dataset. Please provide a valid class attribute")
  if(missing(minority.class))
    minority.class <- which.minority(dataset, class.attr)
  else if(! minority.class %in% names(dataset))
    stop("Minority class not found in dataset")


  minority <- dataset[dataset[, class.attr] == minority.class, ]
  #minority <- as.data.frame( apply(minority, MARGIN=2, factor) )
  attrs <- names(minority)
  attrs <- attrs[attrs != class.attr]
  minority <- minority[, attrs]

  DT <- bnlearn::chow.liu(minority)$arcs
  # Choose only one direction for the arcs (the odd ones) and make tree directed
  tree <- unname(DT[ seq(1, nrow(DT), 2), ])
  make.directed(tree)

  # Calculate conditioned probability distributions
  # Cols are variables to which we are conditioning to
  cond.probs <- apply(tree, MARGIN = 1, function(r){
    table(minority[, r[2]], minority[, r[1]])
  })

  # Calculate absolute probability distributions
  absolute.probs <- apply(minority, MARGIN = 2, function(col){
    table(col)
  })
  absolute.probs <- lapply(absolute.probs, function(x){ x/sum(x) })


  new.samples <- list()

  # For each minority example, create (iterations - burn.in.period)/lag
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
          r <- cond.probs[[k]][toString(x[, tree[,2][k]]), ]
          r/sum(r) * absolute.probs[[attr]]
        })

        second <- sapply(conditioning, function(k){
          r <- cond.probs[[k]][, toString(x[, tree[,1][k]])]
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
  new.samples <- do.call(rbind, new.samples)
  new.samples[, class.attr] <- minority.class
  new.samples
}
