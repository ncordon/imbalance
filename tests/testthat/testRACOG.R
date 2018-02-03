library(testthat)
# Loads data
data(ecoli1)
data(glass0)
data(haberman)
data(iris0)
data(newthyroid1)
data(wisconsin)
data(yeast4)

context("RACOG and wRACOG testing")

####################################################################
# Tests for RACOG
####################################################################
racogTestOutput <- function(d){
  dataset <- eval(as.name(d))
  instances <- 50
  burnin <- 10
  lag <- 10
  newSamples <- racog(dataset, numInstances = instances,
                      burnin = burnin, lag = lag, classAttr = "Class")

  test_that(paste("Correct structure of examples from RACOG and dataset", d), {
    expect_equal(nrow(newSamples), instances)
    expect_equal(as.character(unique(newSamples$Class)), "positive")
  })
}

racogTestOutput("haberman")
racogTestOutput("iris0")
racogTestOutput("newthyroid1")
#racogTestOutput("wisconsin")
#racogTestOutput("ecoli1")
racogTestOutput("yeast4")


test_that("Check of parameters is properly done in RACOG", {
  expect_error(racog(iris0, classAttr = "Species"))
  expect_error(racog(iris0, burnin = "foo"))
  expect_error(racog(iris0, lag = "foo"))
  expect_error(racog(iris0, iterations = "foo"))
})

####################################################################
# Tests for wRACOG
####################################################################

myWrapper <<- structure(list(), class="MyC50Wrapper")
trainWrapper.MyC50Wrapper <<- function(wrapper, train, trainClass){
  C50::C5.0(train, trainClass)
}

dummyWrapper <<- structure(list(), class="DummyWrapper")
trainWrapper.DummyWrapper <- function(wrapper, train, trainClass){
  NULL
}

wracogTestOutput <- function(d){
  dataset <- eval(as.name(d))
  trainFold <- sample(1:nrow(dataset), nrow(dataset)/2, replace = F)
  train <- dataset[trainFold, ]
  validation <- dataset[-trainFold, ]

  test_that(paste("wRACOG executes without error on dataset", d), {
    expect_error(wracog(train, validation, myWrapper), NA)
  })
}

wracogTestOutput("ecoli1")
wracogTestOutput("glass0")
wracogTestOutput("haberman")
wracogTestOutput("iris0")
wracogTestOutput("newthyroid1")
wracogTestOutput("wisconsin")
wracogTestOutput("yeast4")


test_that("Check of parameters is properly done in wRACOG", {
  expect_error(wracog(iris0, iris0, myWrapper, classAttr = "foo"))
  expect_error(wracog(iris0, iris0, C50::C5.0, classAttr = "Class"))
  expect_error(wracog(iris0, iris0, myWrapper, slideWin = "foo"))
  expect_error(wracog(iris0, iris0, myWrapper, slideWin = -1))
  expect_error(wracog(iris0, iris0[,-1], myWrapper, classAttr = "Class"))
  expect_error(wracog(iris0, iris0, dummyWrapper, classAttr = "Class"))
})

