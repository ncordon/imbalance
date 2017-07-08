pdfos <- function(dataset, ratio){
  classes <- unique(dataset$class)
  classes.counts <- sapply(classes, function(c){ length(which(dataset$class==c)) })
  minority.class <- classes[ which.min(classes.counts) ]

  minority <- dataset[dataset$class == minority.class, ]
  minority.size <- nrow(minority)
  minority.class <- unique(minority["class"])
  # Delete class attribute
  minority <- minority[, names(minority) != "class"]
  
  # Total new instances to generate
  n.instances <- minority.size * ratio
  n <- nrow(dataset)
  #n.sqrt <- sqrt(n)

  
  
  # samples.mean <- apply(samples, MARGIN=2, mean)
  
  # Covariance of the positive class
  minority.cov <- cov(minority[, names(minority) != "class"])
  cov.inv.sqrt <- minority.cov**(-1/2)
  
  # Kernel function
  phi <- function(sigma, vector){
    m <- length(vector)
    sigma**(-m) / (2*pi)**(m/2) * exp(-(1/2*sigma**2)* as.numeric(vector %*% vector))
  }
  
  # Cross value function to minimize
  cross.val.score <- function(sigma){
    value <- 2*minority.size**(-1) * phi(sigma, 0)
    
    value <- value + 
      sum(apply(minority, MARGIN=1, function(row.i){
        apply(minority, MARGIN=1, function(row.j){
          ev.point <- as.vector( cov.inv.sqrt %*% (row.j - row.i) )
          phi(sqrt(2) * sigma,  ev.point) - 2*phi(sigma, ev.point)
        })
      })) * (minority.size**(-2))
    value
  }
  
  # grid search to find sigma parameter
  grid <- seq(from = 0, to = 2, by = 0.05)
  # erase 0 from grid search
  grid <- grid[2:length(grid)]
  min.index <- which.min( sapply(grid, function(x){ cross.val.score(x) }) )
  
  opt.sigma <- grid[ min.index ]

  samples <- minority[sample(1:minority.size, n.instances, replace = T), ]
  # Choleski form of covariance matrix, upper triangular matrix
  cov.choleski <- chol(minority.cov)
  
  new.samples <- apply(samples, MARGIN=1, function(row){
    row + t( opt.sigma * cov.choleski %*% rnorm( length(row), mean = 0, sd = 1) )
  })
  
  new.samples <- t(new.samples)
  
  new.samples <- data.frame(new.samples)
  names(new.samples) <- names(minority)
  # Add minority class attribute
  new.samples["class"] <- minority.class
  
  new.samples
}
