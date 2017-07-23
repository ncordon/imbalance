data(datasets)


# Make a factor variable numerical
abalone19$Sex <- sapply(abalone19$Sex, function(x){ ifelse(x=="M", 0, 1) })


classToNumeric <- function(dataset, classAttr = "Class"){
  minClass<- whichMinorityClass(dataset)

  dataset[, classAttr] <- sapply(dataset[, classAttr], function(x){
    ifelse(x == minClass, 1, 0)
  })
  dataset
}


discretizeDataset <- function(dataset, classAttr = "Class"){
  classes <- dataset[, classAttr]
  dataset <- discretize(dataset[, names(dataset) != classAttr])

  for(name in names(dataset)){
    dataset[,name] <- as.numeric(dataset[,name])
  }

  dataset[, classAttr] <- as.factor(classes)
  dataset
}

abalone19 <- classToNumeric(abalone19)
abalone19 <- discretizeDataset(abalone19)



racog(abalone19, burnInPeriod = 10, lag = 10,
      iterations = 300, classAttr = "Class")

