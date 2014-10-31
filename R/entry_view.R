#' Display vectors, lists or rows of a data frames in key-value-pairs.
#'
#' Color coded according to class of contents.
#'
#' @param x List or data frame.
#' @param i Row number to show. Press down/up to browse.
#' @return Nothing.
#' @examples
#' entry.view(Sys.getenv())
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
entry.view <- function(x, i=1){
    x.names <- if(is.null(names(x))) seq_along(x) else names(x)
    nc <- max(nchar(x.names))

    browsing <- TRUE
    while(browsing){
        cat("\n")
        cat(sprintf(sprintf("%%%is: %%s", nc),
                    x.names,
                    sapply(if(!is.null(nrow(x))) x[i,] else x,
                           function(x) style.auto(x, x))),
            sep="\n")
        if(!is.null(nrow(x)) && nrow(x) > 1){
            inchar <- readline(sprintf("\nRow %i of %i. (n)ext, (p)revious, (f)irst, (l)ast, [number]: ",
                                       i, nrow(x)))
            i <- switch(tolower(inchar), n = i+1, p = i-1, f = 1, l = nrow(x),
                        as.integer(inchar))
            browsing <- !is.na(i) && 1 <= i && i <= nrow(x)
        } else {
            browsing <- FALSE
        }
    }
}
