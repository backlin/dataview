##' Wrapper for several methods to test if a variable is empty
##'
##' @param x A variable.
##' @param false.triggers Whether \code{FALSE} should be considered as empty.
##' @return Logical telling if variable is blank.
##' @examples
##' is.blank(NULL)
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @noRd
is.blank <- function(x, false.triggers=FALSE){
    return(
        is.null(x) ||
        length(x) == 0 ||
        all(is.na(x)) ||
        all(x=="") ||
        (false.triggers && all(!x))
    )
}

##' Human readable object dimensions
##'
##' @param x Object.
##' @param use.names Whether to include names of the dimensions.
##' @return A string describing the dimension of an object e.g. `scalar' or `50x2'
##' @examples
##' data(iris)
##' dimfun(iris)
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @noRd
dimfun <- function(x, use.names=FALSE) {
    if(!is.null(dim(x))){
        if(use.names && !is.null(names(dimnames(x)))){
            return(paste(names(dimnames(x)), "(", dim(x), ")", sep="", collapse=" x "))
        } else {
            return(paste(dim(x), collapse="x"))
        }
    } else if(is.vector(x) || is.factor(x)) {
        if(length(x) == 1 && !is.list(x)) return("scalar") else return(paste("#", length(x), sep=""))
    } else if(isS4(x)){
        return(paste("#", length(slotNames(x)), sep=""))
    } else return("")
}

##' Generic object fetching for both S3 and S4
##'
##' @param envir Environment, list or object to search in.
##' @param name of object to look for.
##' @return The object with the given name regardless of if it is located in a
##'   list, environment or S4 object.
##' @examples
##' data(iris)
##' objfun(iris, "Species")
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @noRd
objfun <- function(envir, name){
    return(if(is.environment(envir)){
        get(name, envir=envir)
    } else if(isS4(envir)){
        slot(envir, name)
    } else {
        envir[[name]]
    })
}

##' Human readable object size
##'
##' @param x Object
##' @return A two elemt string vector describing the memory occupied by the
##'   variable e.g. c('32.0', 'B') or c('14.5', 'MiB')
##' @examples
##' data(iris)
##' sizefun(iris)
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @noRd
sizefun <- function(x) {
    if(object.size(x) == 0){
        return(c("0", ""))
    } else {            
        i <- trunc(log(object.size(x)) / log(1024))
        return(c(exp(log(object.size(x)) - i*log(1024)), c("B", "KiB", "MiB", "GiB", "TiB")[i+1]))
    }
}

##' Printf
##'
##' @param ... Sent to \code{sprintf}
##' @return Nothing
##' @examples
##' printf("I have %i %s, oh boy!\n", 74, "bananas")
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @noRd
printf <- function(...) cat(sprintf(...))

##' Trim off leading and trailing whitespaces from a string
##'
##' @param str String.
##' @return A trimmed version of the input string.
##' @examples
##' cat(sprintf("[%%s] vs. [%%s]\n",
##'     sprintf("%%10g", 123.456), trim(sprintf("%%10g", 123.456))
##' ))
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @noRd
trim <- function(str) sub("^\\s*(.*?)\\s*$", "\\1", str)

