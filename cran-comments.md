## Patch
* Addresses ERROR with class(matrix) returning matrix and array for R-devel
* Solves a NOTE about use of SHLIB_OPENMP flags in Makefile
* Solves a WARNING in .cpp files regarding an abort arising in the compiled code

## Test environments
* ubuntu 14.04 (on travis-ci), devel and release
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.

## Downstream dependencies
I have also run R CMD check on downstream dependencies of imbalance:
    
    * No ERRORs or WARNINGs found

