pdfos <- function(dataset, ratio){
  classes <- unique(dataset$class)
  classes.counts <- sapply(classes, function(c){ length(which(dataset$class==c)) })
  minority.class <- classes[ which.min(classes.counts) ]

  minority <- dataset[dataset$class == minority.class, ]
  minority.size <- nrow(minority)
  # Total new instances to generate
  n.instances <- minority.size * ratio
  n <- nrow(dataset)
  #n.sqrt <- sqrt(n)

  samples <- minority[sample(1:minority.size, n.instances, replace = T), names(samples) != "class" ]
  
  # samples.mean <- apply(samples, MARGIN=2, mean)
  
  # Covariance of the positive class
  samples.cov <- cov(minority[, names(minority) != "class"])
  
  
  # Kernel function
  phi <- function(sigma, vector){
    m <- length(vector)
    sigma**(-m) / (2*pi)**(m/2) * exp(-(1/2*sigma**2)* as.numeric(vector %*% vector))
  }
  
  cross-val.score <- function(sigma){
    value <- 2*n.instances**(-1) * phi(sigma, 0)
    
    apply(samples, MARGIN=1, function(row.i){
      apply(samples, MARGIN=1, function(row.j){
        phi(sqrt(2)*sigma, S^(-1/2)*(row.j - row.i)) - 2*phi(sigma, S^(-1/2)*(row.j - row.i))
      })
    })
  }
  
  
  
  new.samples <- apply(samples, MARGIN=1, function(row){
  })

  new.samples
}
