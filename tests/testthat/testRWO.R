# Loads data
data(ecoli1)
data(haberman)
data(iris0)
data(yeast4)
data(newthyroid1)
data(wisconsin)
data(glass0)

context("RWO testing")

####################################################################
# Tests for RACOG
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

rwoTestOutput("glass0")
rwoTestOutput("haberman")
rwoTestOutput("ecoli1")
rwoTestOutput("newthyroid1")


test_that("Check of parameters is properly done in RWO", {
  expect_error(rwo(iris0, classAttr = "Species"))
  expect_error(rwo(iris0, numInstances = "foo"))
  expect_error(rwo("foo", numInstances = 100))
})
