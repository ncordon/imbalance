pdfos <- function(dataset, n.instances){
  # Variance of the positive class
  dataset.var <- var(dataset)
  var.inv.sqrt <- dataset.var ** (-1/2)
  m <- dim(dataset)[2]
  n <- nrow(dataset)

  # Kernel function
  phi <- function(sigma, vector){
    sigma ** (-m) / (2 * pi) ** (m/2) * exp(-(1/2 * sigma ** 2) * as.numeric(vector %*% vector))
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
