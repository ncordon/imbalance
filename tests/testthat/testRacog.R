data(datasets)

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
