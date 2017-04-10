

racog <- function(dataset, burn.in.period, lag, iterations){
  classes <- unique(dataset$class)
  classes.counts <- sapply(classes, function(c){ length(which(dataset$class==c)) })
  minority.class <- classes[ which.min(classes.counts) ]

  minority <- dataset[dataset$class == minority.class, ]

  attrs <- names(minority)
  attrs <- attrs[attrs != "class"]
  minority <- minority[, attrs]


  DT <- bnlearn::chow.liu(minority)$arcs
  tree <- unname(DT[ seq(1, nrow(DT), 2), ])

  # Reverse arcs in tree if not adirected tree
  if(length(unique(tree[,2])) < nrow(tree)){
    tree <- tree[,c(2,1)]
  }

  # Calculate prob distributions
  # Cols are variables to which we are conditioning to
  probs <- apply(tree, MARGIN = 1, function(x){
    table(dataset[,x[2]], dataset[,x[1]])
  })


  new.samples <- list()

  for(i in 1:nrow(minority)){
    x <- minority[i,]

    for(t in 1:iterations){
      for(attr in attrs){
        from <- which(tree[,1] == attr)
        to <- which(tree[,2] == attr)

        first <- sapply(from, function(k){
          r <- probs[[k]][ x[, tree[,2][k]], ]
          r/sum(r)
        })

        second <- sapply(to, function(k){
          r <- probs[[k]][ ,x[, tree[,1][k]]]
          r/sum(r)
        })

        prob.vectors <- cbind(first, second)


        ith.prob <- apply(prob.vectors, MARGIN = 1, function(r){
          prod(unlist(r))
        })

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
