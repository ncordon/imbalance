balance.dataset <- function( dataset, ratio, method = c("pdfos", "racog"), class.attr = "class"){
  # Check of arguments
  if (missing(dataset))
    stop("dataset must not be empty")
  if (missing(ratio) || !is.numeric(ratio) || ratio < 1 )
    stop("ratio must be a number greater or equal than 1")
  if ( !(class.attr %in% names(dataset)) )
    stop(paste("Class attribute '", class.attr, "' not found in dataset", sep=""))
  
  
  classes <- unique(dataset[, class.attr])
  classes.counts <- sapply(classes, function(c){ length(which(dataset[, class.attr]==c)) })
  minority.class <- classes[ which.min(classes.counts) ]
  
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


pdfos <- function(dataset, n.instances){
  # Variance of the positive class
  dataset.var <- var(dataset)
  var.inv.sqrt <- dataset.var**(-1/2)
  m <- dim(dataset)[2]
  n <- nrow(dataset)
    
  # Kernel function
  phi <- function(sigma, vector){
    sigma**(-m) / (2*pi)**(m/2) * exp(-(1/2*sigma**2)* as.numeric(vector %*% vector))
  }
  
  
  # Cross value function to minimize
  cross.val.score <- function(sigma){
    value <- 2/n * phi(sigma, 0)
    
    value <- value + 
      sum(apply(dataset, MARGIN=1, function(row.i){
        apply(dataset, MARGIN=1, function(row.j){
          ev.point <- as.vector( var.inv.sqrt %*% (row.j - row.i) )
          phi(sqrt(2) * sigma,  ev.point) - 2 * phi(sigma, ev.point)
        })
      })) / (n * n)
    
    value
  }
  
  
  # grid search to find approximation to best sigma parameter
  grid <- seq(from = 0, to = 2, by = 0.05)
  # erase 0 from grid search
  grid <- grid[2:length(grid)]
  min.index <- which.min( sapply(grid, function(x){ cross.val.score(x) }) )
  
  opt.sigma <- grid[ min.index ]

  # Choleski form of variance matrix, upper triangular matrix
  var.choleski <- chol(dataset.var)
  
  samples <- dataset[sample(1:n, n.instances, replace = T), ]
  new.samples <- apply(samples, MARGIN=1, function(row){
    row + t( opt.sigma * var.choleski %*% rnorm( m, mean = 0, sd = 1) )
  })
  
  new.samples <- t(new.samples)
  new.samples <- data.frame(new.samples)
  new.samples
}


resultado <- balance.dataset(dataset = iris, ratio=1, method = "pdfos", class.attr = "Species")
