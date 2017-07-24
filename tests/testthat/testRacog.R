data(datasets)


# Make a factor variable numerical
abalone19$Sex <- sapply(abalone19$Sex, function(x){ ifelse(x=="M", 0, 1) })


classToNumeric <- function(dataset, classAttr = "Class"){
  minClass<- whichMinorityClass(dataset)

  dataset[, classAttr] <- as.factor(
    sapply(dataset[, classAttr], function(x){
      ifelse(x == minClass, 1, 0)
    })
  )
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


undiscretizeDataset <- function(dataset, discretizedDataset, newSamples, classAttr = "Class"){
  minorityClass <- newSamples[1, classAttr]
  whichAreMinority <- dataset[, classAttr] == minorityClass
  discretizedDataset <- discretizedDataset[whichAreMinority, ]
  dataset <- dataset[whichAreMinority, ]

  newCols <- lapply(1:ncol(newSamples), function(c){
    sapply(newSamples[,c], function(v){
      sample(dataset[ discretizedDataset[,c] == v, c], 1)
    })
  })

  newDataset <- do.call(cbind.data.frame, newCols)
  colnames(newDataset) <- names(newSamples)
  newDataset
}


abalone19 <- classToNumeric(abalone19)
abaloneDiscretized <- discretizeDataset(abalone19)


# length(which(abalone19$Class==1))
# Creates (1000-10)/10 * 32 = 3168 examples of positive class
newSamples <- racog(abaloneDiscretized, burnInPeriod = 10, lag = 10,
      iterations = 1000, classAttr = "Class")
newSamples <- undiscretizeDataset(abalone19, abaloneDiscretized, newSamples)
modifiedDataset <- do.call(rbind.data.frame, list(abalone19, newSamples))

# qplot(Length, Shell_weight, data = abalone19, col = Class)
# qplot(Length, Shell_weight, data = modifiedDataset, col = Class)
plotComparison(abalone19, modifiedDataset, "Class", col=2, c("Length", "Diameter"))
