## Patch
This is a patch to ensure C++ portability in relation to an ERROR that arose with the Solaris checks on CRAN

## Test environments
* local OS X, devel
* ubuntu 14.04 (on travis-ci), devel and release
* local Arch Linux 4.9.56-1-lts, R 3.4.2
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.

Apart from that NOTE, there were some complains about mis-spelled words in DESCRIPTION: datasets and preprocessing, which I believe are technical field words.

## Downstream dependencies
I have also run R CMD check on downstream dependencies of imbalance:
    
    * No ERRORs or WARNINGs found

