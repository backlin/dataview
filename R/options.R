#' Retrieves package options
#' 
#' @param x Option to retrieve.
#' @examples
#' # This shows how to modify a column presented by whos.
#' # The new function only reports the size of non-S4 objects
#' # to improve execution time.
#' opt <- default.options()
#' opt$columns$bytes <- function(x) if(isS4(x)) NA else object.size(x)
#' options(synesthesia = opt)
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
getOpt <- function(x){
    ifnull(getOption("synesthesia")[[x]],
           default.options()[[x]])
}
#' @rdname getOpt
#' @export
default.options <- function(){
    list(
        align = c(bytes="right"),
        columns = list(
            class = function(x){
                cls <- class(x)
                paste0(cls[1],
                       if(length(cls) > 1) sprintf(" (+%i)", length(cls)-1)
                       else "", sep="")
            },
            S4 = isS4,
            Key = key,
            dim = function(x){
                if(is.function(x)){
                    NA
                } else {
                    x <- list(length = length(x), dim = dim(x))
                    if(is.null(x$dim)){
                        as.character(x$length)
                    } else {
                        paste(x$dim, collapse="x")
                    }
                }
            },
            bytes = object.size,
            comment = comment
        ),
        print = list(
            bytes = function(x){
                unit <- c("B  ", "B  ", "KiB", "MiB", "GiB", "TiB", "EiB")[
                    1+sapply(x, function(b) sum(b > 1024^(0:4)))]
                size <- ifelse(x == 0, 0, 2^(log2(x) %% 10))
                sprintf("%.4g %s", size, unit)
            }
        ),
        summary = list(bytes = sum),
        NULL
    )
}
