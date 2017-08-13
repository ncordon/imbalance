library(testthat)
# Loads data
data(ecoli1)
data(glass0)
data(haberman)
data(iris0)
data(newthyroid1)
data(wisconsin)
data(yeast4)

context("PDFOS testing")

####################################################################
# Tests for PDFOS
####################################################################
pdfosTestOutput <- function(d){
  dataset <- eval(as.name(d))
  nInstances <- 100
  newSamples <- pdfos(dataset, numInstances = nInstances, classAttr = "Class")

  test_that(paste("Correct structure of examples from PDFOS and dataset", d), {
    expect_equal(nrow(newSamples),  nInstances)
    expect_equal(as.character(unique(newSamples$Class)), "positive")
  })
}

pdfosTestOutput("glass0")
pdfosTestOutput("haberman")
pdfosTestOutput("iris0")
pdfosTestOutput("newthyroid1")
# pdfosTestOutput("wisconsin")
# pdfosTestOutput("ecoli1")
# pdfosTestOutput("yeast4")

test_that("Check of parameters is properly done in PDFOS", {
  expect_error(pdfos(iris0, classAttr = "Species"))
  expect_error(pdfos(iris0, numInstances = "foo"))
  expect_error(pdfos("foo", numInstances = 100))
})
