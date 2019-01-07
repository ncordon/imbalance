
<!-- README.md is generated from README.Rmd. Please edit that file -->

# imbalance

[![Build
Status](https://travis-ci.org/ncordon/imbalance.svg?branch=master)](https://travis-ci.org/ncordon/imbalance)
[![minimal R
version](https://img.shields.io/badge/R%3E%3D-3.3.0-6666ff.svg)](https://cran.r-project.org/)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/imbalance)](https://cran.r-project.org/package=imbalance)
[![packageversion](https://img.shields.io/badge/Package%20version-1.0.1-orange.svg?style=flat-square)](https://github.com/ncordon/imbalance/commits/master)

`imbalance` provides a set of tools to work with imbalanced datasets:
novel oversampling algorithms, filtering of instances and evaluation of
synthetic instances.

## Installation

You can install imbalance from Github with:

``` r
# install.packages("devtools")
devtools::install_github("ncordon/imbalance")
```

## Examples

Run `pdfos` algorithm on `newthyroid1` imbalanced dataset and plot a
comparison between attributes.

``` r
library("imbalance")
data(newthyroid1)

newSamples <- pdfos(newthyroid1, numInstances = 80)
# Join new samples with old imbalanced dataset
newDataset <- rbind(newthyroid1, newSamples)
# Plot a visual comparison between both datasets
plotComparison(newthyroid1, newDataset, attrs = names(newthyroid1)[1:3], cols = 2, classAttr = "Class")
```

![](README-example-pdfos-1.png)<!-- -->

After filtering examples with `neater`:

``` r
filteredSamples <- neater(newthyroid1, newSamples, iterations = 500)
#> [1] "12 samples filtered by NEATER"
filteredNewDataset <- rbind(newthyroid1, filteredSamples)
plotComparison(newthyroid1, filteredNewDataset, attrs = names(newthyroid1)[1:3])
```

![](README-example-neater-1.png)<!-- -->

Execute method `ADASYN` using the wrapper provided by the package,
comparing imbalance ratios of the dataset before and after oversampling:

``` r
imbalanceRatio(glass0)
#> [1] 0.4861111
newDataset <- oversample(glass0, method = "ADASYN")
imbalanceRatio(newDataset)
#> [1] 0.9722222
```
