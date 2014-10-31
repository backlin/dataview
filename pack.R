library("roxygen2")
roxygenize()
system("R CMD build .")
f <- file.info(dir(, "*\\.tar\\.gz"))
new.build <- rownames(f)[which.max(f$mtime)]
system(sprintf("R CMD check %s --as-cran", new.build))
system(sprintf("R CMD INSTALL %s", new.build))
