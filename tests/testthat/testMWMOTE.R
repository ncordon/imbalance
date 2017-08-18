library(testthat)
# Loads data
data(ecoli1)
data(glass0)
data(haberman)
data(iris0)
data(newthyroid1)
data(wisconsin)
data(yeast4)

context("MWMOTE testing")

####################################################################
# Tests for MWMOTE
####################################################################
mwmoteTestOutput <- function(d){
  dataset <- eval(as.name(d))
  nInstances <- 100
  newSamples <- mwmote(dataset, numInstances = nInstances, classAttr = "Class")

  test_that(paste("Correct structure of examples from MWMOTE and dataset", d), {
    expect_equal(nrow(newSamples),  nInstances)
    expect_equal(as.character(unique(newSamples$Class)), "positive")
  })
}

mwmoteTestOutput("ecoli1")
mwmoteTestOutput("glass0")
mwmoteTestOutput("haberman")
mwmoteTestOutput("iris0")
mwmoteTestOutput("newthyroid1")
mwmoteTestOutput("wisconsin")
mwmoteTestOutput("yeast4")

test_that("Check of parameters is properly done in MWMOTE", {
  expect_error(mwmote(iris0, classAttr = "Species"))
  expect_error(mwmote(iris0, numInstances = "foo"))
  expect_error(mwmote("foo", numInstances = 100))
  expect_error(mwmote(iris0, numInstances = 100, kNoisy = -1))
  expect_error(mwmote(iris0, numInstances = 100, threshold = 0))
})
