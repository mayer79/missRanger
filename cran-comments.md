This is a maintenance release, switching to 

- testthat 3,
- modifying vignette order,
- improving the way how the package is being updated/generated.

## R CMD check results seem okay

checking for unstated dependencies in examples ... OK
   WARNING
  'qpdf' is needed for checks on size reduction of PDFs

## Online check results seem okay (2 notes below)

- check_win_devel()
- check_rhub()

Found the following (possibly) invalid DOIs:
  DOI: 10.1093/bioinformatics/btr597
    From: DESCRIPTION
    Status: Forbidden
    Message: 403
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
