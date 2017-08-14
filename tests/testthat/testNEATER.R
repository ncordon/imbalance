library(testthat)
# Loads data
data(ecoli1)
data(glass0)
data(haberman)
data(iris0)
data(newthyroid1)
data(wisconsin)
data(yeast4)

context("NEATER testing")

####################################################################
# Tests for PDFOS
####################################################################
neaterTestOutput <- function(d){
  dataset <- eval(as.name(d))
  nInstances <- 100
  minIndex <- which(names(dataset) == "Class")
  newSamples <- smotefamily::SMOTE(dataset[, -minIndex], dataset[, minIndex])$syn_data
  attrs <- names(newSamples)
  names(newSamples) <- c(attrs[attrs != "class"], "Class")

  test_that(paste("NEATER executes without error on dataset", d), {
    expect_error(neater(dataset, newSamples, k = 3, iterations = 100,
                        smoothFactor = 1, classAttr = "Class"), NA)
    })
}

# Test that NEATER returns correctly
neaterTestOutput("glass0")
neaterTestOutput("haberman")
neaterTestOutput("iris0")
neaterTestOutput("newthyroid1")
# neaterTestOutput("wisconsin")
# neaterTestOutput("ecoli1")
# neaterTestOutput("yeast4")

test_that("Check of parameters is properly done in NEATER", {
  expect_error(neater(iris0, iris0, classAttr = "Species"))
  expect_error(neater(iris0, iris0, iterations = -1))
  expect_error(neater("foo", k = -2))
  expect_error(neater(iris0, iris0[,-1]))
})
