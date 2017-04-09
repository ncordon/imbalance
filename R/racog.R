
# Rings is the class attribute for abalone
abalone <- read.csv2("../Data/abalone.data", header=T, sep=",")
# class is the class attribute for cars
cars <- read.csv2("../Data/car.data", header=T, sep=",")

# No funciona con datasets no categóricos
bnlearn::chow.liu(abalone)
head(abalone)
head(cars)
cars.tree <- bnlearn::chow.liu(cars)
cars.tree$arcs
dataset <- cars





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
    contingency <- table(dataset[,x[2]], dataset[,x[1]])
    apply(contingency, MARGIN=2, function(x){ x/sum(x) })
  })


  apply(minority, MARGIN=1, function(x)
    for(t in 1:iterations){
      for(i in 1:size(attrs)){
        from <- which(tree[,1] == attrs[i])
        to <- which(tree[,2] == attrs[i])

        #sapply(from, probs[,x[i]])

      }

      x.prev <- x
    }
  })

}
