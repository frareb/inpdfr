# package inpdfr

## Resubmission
This is a warnings-fix release. In this version I have:
* used VCorpus instead of Corpus
* readLines instead of readChar
* NOT fixed running examples in 'doCA' (Running examples failed)

## Test environments
* Windows 10, R version 3.6.2
* Linux Xubuntu 19.10, R version 3.6.1
* Windows, R-devel 2020-01-07 r77633
* TRAVIS with Ubuntu 16.04 oldrel
* TRAVIS with Ubuntu 16.04 release
* TRAVIS with Ubuntu 16.04 rdevel 2020-01-14 r77665

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs with all testing environments except
Linux rdevel 2020-01-14 r77665. The problem seems to be the "ca"
package on which "inpdfr" depends (see https://cran.r-project.org/web/checks/check_results_ca.html).

Please let me know if there is anything I can do (skip example in doCA?).
Best regards.
