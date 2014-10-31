library("roxygen2")
roxygenize()
system("R CMD build .")
new.build <- rownames(f)[which.max(f$mtime)]
f <- file.info(dir(, "*\\.tar\\.gz"))
system(sprintf("R CMD check %s --as-cran", f))

system("R CMD INSTALL .")

