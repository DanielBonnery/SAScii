# SAScii
Import ASCII files directly into R using only a SAS input script

This package was forked from ajdamico/SAScii.
The principal differences with ajdamico/SAScii at the time it was forked:

* add a namespace/description so that it can be installed without error via the R commands:
```r
devtools::install_github("DanielBonnery/SAScii")
```
* Change the read.fwf to readr::read_fwf to speed up the conversion of dat files to R files (save hours)

It works to download CPS data, for other datasets it was not tested, so it is recommended to use ajdamico/SAScii.



