library(testthat)
# Loads data
data(ecoli1)
data(glass0)
data(haberman)
data(iris0)
data(newthyroid1)
data(wisconsin)
data(yeast4)

context("RWO testing")

####################################################################
# Tests for RWO
####################################################################
rwoTestOutput <- function(d){
  dataset <- eval(as.name(d))
  nInstances <- 100
  newSamples <- rwo(dataset, numInstances = nInstances, classAttr = "Class")

  test_that(paste("Correct structure of examples from RWO and dataset", d), {
    expect_equal(nrow(newSamples),  nInstances)
    expect_equal(as.character(unique(newSamples$Class)), "positive")
  })
}

rwoTestOutput("ecoli1")
rwoTestOutput("glass0")
rwoTestOutput("haberman")
rwoTestOutput("iris0")
rwoTestOutput("newthyroid1")
rwoTestOutput("wisconsin")
rwoTestOutput("yeast4")

test_that("Check of parameters is properly done in RWO", {
  expect_error(rwo(iris0, classAttr = "Species"))
  expect_error(rwo(iris0, numInstances = "foo"))
  expect_error(rwo("foo", numInstances = 100))
})
