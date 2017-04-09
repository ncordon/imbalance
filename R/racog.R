
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
  tree <- DT[ seq(1, nrow(DT), 2), ]


  apply(minority, MARGIN=1, function(row){
    for(t in 1:iterations){
      for(i in 1:size(attrs)){
        from <- tree[,2][tree[,1] == attrs[i]]
        to <- tree[,1][tree[,2] == attrs[i]]
      }
    }
  })

}
