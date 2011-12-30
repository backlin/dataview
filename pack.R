cd("~/Documents/R/egna paket/dataview")
library("roxygen2")
#options(useFancyQuotes = FALSE)

roxygen.update.description()
roxygenize("dataview", "dataview.roxygen", unlink.target=TRUE)
system("rm -rf dataview.roxygen/inst")
system("R CMD check dataview.roxygen")

system("R CMD INSTALL dataview.roxygen")

system("R CMD build dataview.roxygen") # Build package

#system("R CMD check dataview.roxygen --use-gct") # Check package with GC-torture
#system("R CMD INSTALL --build --clean dataview.roxygen") # Build binary

