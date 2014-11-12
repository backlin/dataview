#' Browse the contents of a nested data structure
#' 
#' Manually step in and out of the elements of complex data structure with
#' \code{\link{whos}}. To move around enter the name or number of the element
#' you want to inspect next. Partial names will automatically be matched against
#' the possible element names.
#' 
#' @param x Object.
#' @param name Name of the object.
#' @return Nothing.
#' @examples
#' \dontrun{
#' require(ggplot2)
#' p <- ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width)) +
#'     geom_point()
#' browse(p)
#' }
#' @seealso whos
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
browse <- function(x=.GlobalEnv, name){
    if(missing(name)) name <- deparse(substitute(x))
    elem <- NA
    w <- whos(x)

    # I haven't yet figured out why browse cannot find the [.whos if it is not
    # specified here too... weird
    `[.whos` <- function(x, ...){
        structure(as.data.table(x)[...], class=class(x))
    }

    if(!identical(x, .GlobalEnv)){
        attach(x) # To get tab completion
        on.exit(detach(x))
    }

    while(is.na(elem) || elem != ""){
        cat("\nBrowsing ", name, "\n", sep="")
        print(w)
        elem <- readline("Select an element to inspect: ")
        if(elem != ""){
            if(grepl("^\\d+$", elem) && !elem %in% w$name){
                i <- as.integer(elem)
                if(is.na(w$name[i])){
                    elem <- if(i > nrow(w)) NA else i
                } else {
                    elem <- w$name[i]
                }
            } else {
                if(!elem %in% w$name){
                    i <- grep(sprintf("^%s", elem), w$name)
                    while(length(i) > 1){
                        cat("\n")
                        print(w[i])
                        elem <- readline("Please specify: ")
                        if(grepl("^\\d+$", elem) && !elem %in% w$name[i]){
                            i <- i[as.integer(elem)]
                        } else {
                            i <- grep(sprintf("^%s", elem), w$name)
                        }
                    }
                    elem <- if(is.na(i) || length(i) == 0) NA else w$name[i]
                }
            }
            if(is.na(elem)){
                cat("No matches, please try again. Enter blank or press ctrl+c to exit.\n")
            } else {
                if(isS4(x)){
                    obj <- slot(x, elem)
                    obj.name <- sprintf("%s@%s", name, elem)
                } else {
                    if(is.integer(elem)){
                        obj <- x[[elem]]
                        obj.name <- sprintf("%s[[%i]]", name, elem)
                    } else {
                        obj <- get(elem, x)
                        obj.name <- sprintf("%s$%s", name,
                            if(elem == make.names(elem)) elem else sprintf("`%s`", elem))
                    }
                }
                if(is.function(obj)){
                    obj <- environment(obj)
                    obj.name <- sprintf("environment(%s)", obj.name)
                }
                tryCatch({
                    suppressWarnings(browse(obj, obj.name))
                }, error = function(...){
                    tryCatch({
                        h <- head(obj)
                        cat("Head of", obj.name, "\n")
                        print(h)
                    }, error = function(...){
                        cat("Could not inspect object.\n")
                    })
                })
            }
        }
    }
}
