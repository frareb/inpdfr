# package inpdfr

## Resubmission
This is a resubmission. In this version I have:
* fixed the URL to download GTK+ in man/loadGUI.Rd
* fixed the NOTE "Strong dependencies not in mainstream repositories: Rstem". Word stem is now performed with the SnowballC package.
* added the missing @export roxygen field for function getwordOccuDF
* added the missing @export roxygen field for function getAllAnalysis

All modifications were commented into the NEWS.md file and package version updated to 0.1.1

## Test environments
* Windows 10, R version 3.2.3
* Linux Debian Jessie, R version 3.2.3
* Linux Ubuntu LTS 12.04, R version 3.2.3
* Windows, R-devel with win-builder.r-project.org

## R CMD check results
There were no ERRORs,WARNINGs, or NOTEs with R 3.2.3 and 1 NOTE with R-devel:

Cheking CRAN incoming feasibility ... NOTE

* Maintainer: 'Rebaudo Francois <francois.rebaudo@ird.fr>'
=> ok

* New submission
=> ok

## Downstream dependencies
There are currently no downstream dependencies for this package.
