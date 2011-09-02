setwd("~/Dropbox/R/egna paket/dataview")

library("roxygen")

opt <- options()
options(useFancyQuotes = FALSE)
roxygenize("dataview", copy.package = TRUE, unlink.target = TRUE,
           use.Rd2 = TRUE)
options(opt)

## NOTE: Do the following manually before proceeding
#  - Fix line breaks on the on the date line in dataview.roxygen/DESCRIPTION
#  - Remove the dataview.roxygen/inst folder
system("rm -rf dataview.roxygen/inst")
system("R CMD check dataview.roxygen") # Check package
system("R CMD INSTALL dataview.roxygen") # Install package


system("R CMD build dataview.roxygen") # Build package




#system("R CMD check dataview.roxygen --use-gct") # Check package with GC-torture

#system("R CMD INSTALL --build --clean dataview.roxygen") # Build binary

