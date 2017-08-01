data(datasets)
context("RACOG and wRACOG testing")

####################################################################
# Tests for RACOG
####################################################################
testOutput <- function(d){
  dataset <- eval(as.name(d))
  datasetDiscretized <- discretizeDataset(dataset)
  newSamples <- racog(datasetDiscretized, iterations = 100,
                      burnin = 10, lag = 10, classAttr = "Class")

  test_that(paste("Correct structure of examples from RACOG and dataset", d), {
    expect_equal(nrow(newSamples), (100-10)/10 * length(which(dataset$Class == 1)))
    expect_equal(as.character(unique(newSamples$Class)), "1")
  })
}

testOutput("ecoli1")
testOutput("haberman")
testOutput("iris0")


test_that("Check of parameters is properly done in RACOG", {
  expect_error(racog(iris0, classAttr = "Species"))
  expect_error(racog(iris0, burnin = "foo"))
  expect_error(racog(iris0, lag = "foo"))
  expect_error(racog(iris0, iterations = "foo"))
})

####################################################################
# Tests for wRACOG
####################################################################
dataNames <- c("ecoli1", "glass", "haberman", "iris0",
               "wisconsin", "yeast4", "yeast6")

myWrapper <- structure(list(), class="C50Wrapper")
trainWrapper <- function(wrapper, train, trainClass){ UseMethod("trainWrapper") }
trainWrapper.C50Wrapper <- function(wrapper, train, trainClass){
  C50::C5.0(train, trainClass)
}

lapply(dataNames, function(d){
  dataset <- eval(as.name(d))
  trainFold <- sample(1:nrow(dataset), nrow(dataset)/2, replace = F)
  train <- dataset[trainFold, ]
  validation <- dataset[-trainFold, ]

  test_that(paste("wRACOG executes without error on dataset", d),{
    expect_error(wracog(train, validation, myWrapper), NA)
  })
})

