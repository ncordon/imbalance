racog <- function(dataset, burnInPeriod, lag, iterations,
                  classAttr = "class", minorityClass){
  if(! classAttr %in% names(dataset))
    stop("class attribute not found in dataset. Please provide a valid class attribute")
  if(missing(minorityClass))
    minorityClass <- whichMinorityClass(dataset, classAttr)
  else if(! minorityClass %in% levels(dataset[, classAttr]))
    stop("Minority class not found in dataset")


  minority <- dataset[dataset[, classAttr] == minorityClass, ]
  #minority <- as.data.frame( apply(minority, MARGIN=2, factor) )
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

  # For each minority example, create (iterations - burnInPeriod)/lag
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

      if(t > burnInPeriod && t%%lag == 0){
        newSamples[[length(newSamples)+1]] <- x
      }
    }
  }

  # Output
  newSamples <- do.call(rbind, newSamples)
  newSamples[, classAttr] <- factor(minorityClass, levels = levels(dataset[, classAttr]))
  newSamples
}
