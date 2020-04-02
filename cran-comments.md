## Test environments
* local OS X install, R 3.6.3
* ubuntu 14.04 (on travis-ci), R 3.6.3
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Response to Swetlana Herbrandt following first CRAN submission.

Thank you for the insightful comments. I have addressed each of your comments.

* The title of the package has been shortened.
* The Description field of the DESCRIPTION file has been shorted and references to the theoretical background of the the methods in the package have been added.
* Every function now includes an example. The practical vingette still serves as the main reference for how to use the package.
* All cat() functions have been replaced with message()

I hope that this is satisfactory.
