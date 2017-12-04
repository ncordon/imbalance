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

imbalanceRatio <- function(dataset, classAttr = "Class"){
  minorityClass <- .whichMinorityClass(dataset, classAttr)
  howMuchMinority <- length(which(dataset$Class == minorityClass))

  howMuchMinority / (nrow(dataset) - howMuchMinority)
}
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
wrapperTestOutputRatio("adaptative")
wrapperTestOutputRatio("SLMOTE")
wrapperTestOutputRatio("borderline-SMOTE")
wrapperTestOutputRatio("density-SMOTE")
#wrapperTestOutputRatio("RACOG")


# test_that("Check of parameters is properly done in MWMOTE", {
#   expect_error(mwmote(iris0, classAttr = "Species"))
#   expect_error(mwmote(iris0, numInstances = "foo"))
#   expect_error(mwmote("foo", numInstances = 100))
#   expect_error(mwmote(iris0, numInstances = 100, kNoisy = -1))
#   expect_error(mwmote(iris0, numInstances = 100, threshold = 0))
# })
