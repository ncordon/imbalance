library(testthat)
# Loads data
data(ecoli1)
data(glass0)
data(haberman)
data(iris0)
data(newthyroid1)
data(wisconsin)
data(yeast4)

context("Test utils(wrapper, ...)")

####################################################################
# Tests for wrapper of methods
####################################################################
wrapperTestOutputRatio <- function(method){
  ratio <- 0.8
  newDataset <- oversample(glass0, ratio = ratio, method = method)

  test_that(paste("Correct functioning of wrapper with ", method), {
    expect_equal(imbalanceRatio(newDataset), ratio, tolerance = 1e-02)
  })
}

wrapperTestOutputRatio("PDFOS")
wrapperTestOutputRatio("RWO")
wrapperTestOutputRatio("MWMOTE")
wrapperTestOutputRatio("SMOTE")
# Be careful!, ADASYN can reach 0.8 of imbalance ratio in glass0, maybe not
# in others dataset because number of synthetic examples cannot be indicated
# to the method of smotefamily
wrapperTestOutputRatio("ADASYN")
wrapperTestOutputRatio("ANSMOTE")
wrapperTestOutputRatio("SLMOTE")
wrapperTestOutputRatio("BLSMOTE")
#wrapperTestOutputRatio("DBSMOTE")
#wrapperTestOutputRatio("RACOG")


test_that("Check of parameters is properly done in wrapper", {
  # ratio not passed to the function
  expect_error(oversample(glass0, method = "MWMOTE"))
  # ADASYN does not need a ratio
  expect_error(oversample(glass0, method = "ADASYN"), NA)
  # ratio cannot be greater than 1
  expect_error(oversample(glass0, ratio = 1.2, method = "SMOTE"))
  # ratio cannot be lower than current ratio of imbalance
  expect_error(oversample(glass0, ratio = 0.1, method = "SMOTE"))
  # ADASYN is not going to be able to achieve that imbalance ratio (does not
  # require such parameter in smotefamily package)
  expect_warning(oversample(glass0, ratio = 0.99, method = "ADASYN"))
  # non-existent wrapper
  expect_error(oversample(glass0, method = "wRACOG", wrapper = "asdf"))
  # Good calls to wRACOG
  #expect_error(oversample(glass0, method = "wRACOG", wrapper = "C5.0"), NA)
  #expect_error(oversample(glass0, method = "wRACOG", wrapper = "KNN"), NA)
})
