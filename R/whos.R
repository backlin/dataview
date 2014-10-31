#' Display contents of an evironment, data.frame or list as a summary table
#'
#' Color coded according to class and dimensions of contents. See
#' \code{\link[xtermStyle]{style}} for details.
#'
#' @param envir Environment, data frame or list to be displayed. Optional,
#'   default: globalenv()
#' @param pattern Regexp filtering of objects. Only objects matching the pattern
#'   are displayed. Optional, default: show all objects.
#' @param exclude A list of objects not to be displayed. To set a default exclusion
#'   mask use the \code{whos.set.mask} function. If \code{whos.set.mask} is
#'   called without a list of object names all objects currently in globalenv()
#'   are hidden. This is useful for example if you have a lot of stuff in the
#'   workspace that you aren't currently interested in but is needed to make
#'   your code run.
#' @return Nothing
#' @examples
#' whos()
#' data(USArrests)
#' whos(USArrests)
#' 
#' whos.set.mask()
#' data(iris)
#' whos()
#' whos.all()
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @import data.table
#' @import xtermStyle
#' @export
whos <- function(envir=parent.frame(), pattern=".", exclude=getOption("whos.exclude")){
    # Interpret the `envir` argument if not already an environment
    envir <- switch(class(envir)[1],
        `character` = {
            include <- envir
            parent.frame()
        },
        `function` = environment(envir),
        `integer` = as.environment(envir),
        `numeric` = {
            if(envir != as.integer(envir)) stop("Invalid `envir` argument")
            as.environment(envir)
        },
        envir
    )
    
    # Get names of objects in environment matching pattern
    obj.name <- if(is.environment(envir)){
        ls(envir=envir)
    } else if(isS4(envir)){
        slotNames(envir)
    } else {
        # lists, data.frames, data.tables etc
        if(!is.null(names(envir))) names(envir) else 1:length(envir)
    }
    if(is.character(obj.name))
        obj.name <- setdiff(grep(pattern, obj.name, value=TRUE), exclude)

    # Present objects
    n <- length(obj.name)
    if(n == 0){
        cat("No objects found\n")
    } else {
        # Make an object/property matrix (objects as rows, properties as columns)
        obj.sapply <- if(isS4(envir)){
            function(fun, ...) sapply(obj.name, function(x) fun(slot(envir, x)), ...)
        } else if(is.character(obj.name)){
            function(fun, ...) sapply(obj.name, function(x) fun(get(x, envir)), ...)
        } else {
            function(fun, ...) sapply(envir, fun, ...)
        }
        obj.lapply <- function(...) obj.sapply(..., simplify=FALSE)

        b <- obj.sapply(object.size)
        structure(data.table(
            name = if(is.character(obj.name)) obj.name else NA,
            class = {
                cls <- obj.lapply(class)
                paste0(sapply(cls, "[", 1),
                       ifelse(sapply(cls, length) > 1,
                              sprintf(" (+%i)", sapply(cls, length)-1),
                              ""))
            },
            S4 = obj.sapply(isS4),
            table.key = if(haskey(envir)) names(envir) %in% key(envir) else FALSE,
            dim = obj.sapply(function(x){
                x <- list(length = length(x), dim = dim(x))
                if(is.null(x$dim)){
                    as.character(x$length)
                } else {
                    paste(x$dim, collapse="x")
                }
            }),
            size = 2^(log2(b) %% 10),
            unit = c("B", "KiB", "MiB", "GiB", "TiB", "EiB")[sapply(b, function(x) sum(x > 1024^(0:4)))],
            comment = obj.sapply(function(x) !is.null(comment(x))),
            style = obj.sapply(style.auto)
        ), class=c("whos", "data.table", "data.frame"))
    }
}

#' @param x \code{\link{whos}} object.
#' @noRd
#' @export
print.whos <- function(x, ...){
    # Determine how many characters each column occupies i.e. length of longest entry in each column
    space <- 2
    nc <- x[, list(
        index = nchar(nrow(x)) + 1 + space,
        name = if(all(is.na(name))) 0 else max(nchar(name)) + space,
        table.key = if(any(table.key)) 5 + space else 0,
        class = max(nchar(class)) + space,
        S4 = if(any(S4)) 4 + space else 0,
        dim = max(nchar(dim)) + space,
        size = 6,
        unit = max(nchar(unit)) + space,
        comment = if(any(comment)) 2 + space
    )]
    sfun <- function(str, width) sprintf(sprintf("%%-%is", width), str)
    cat(x[,paste0(
        sprintf(sprintf("%%%is:", nc$index - 1 - space), seq_len(nrow(x))),
        sprintf(sprintf("%s%%%is", x$style, space), ""),
        if(all(is.na(name))) NULL else sfun(name, nc$name),
        sfun(ifelse(table.key, "[key]", ""), nc$table.key),
        sfun(class, nc$class),
        sfun(ifelse(S4, "[S4]", ""), nc$S4),
        sfun(dim, nc$dim),
        sprintf("%5.4g ", size),
        sfun(unit, nc$unit),
        sfun(ifelse(comment, "+!", ""), nc$comment),
        style.clear()
    )], sep="\n")
}

#' Set a default exclusion mask for \code{\link{whos}}.
#'
#' @param exclude List of object names. These will be hidden from view.
#'   Defaults to all objects in \code{envir}.
#' @param envir Environment to work in.
#' @return Nothing. The mask is stored as `whos.exclude' in the global option list.
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
whos.set.mask <- function(exclude, envir=globalenv()){
    # This function sets the default exclusion mask for the `whos' function.
    # It is stored as a list in the global options. When "whosing" objects fully
    # matching an entry in the mask will be omitted from the output.
    if(missing(exclude)) options(whos.exclude = ls(envir=envir))
    else options(whos.exclude = exclude)
}

#' Shortcut for calling whos without exclusion.
#'
#' @param ... Parameters sent to \code{\link{whos}}.
#' @return Nothing
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @rdname whos
#' @export
whos.all <- function(...){
    whos(..., exclude=NULL)
}

#' @param x \code{\link{whos}} object.
#' @param keep.rownames Ignored, kept for S3 consistency.
#' @noRd
#' @export
as.data.table.whos <- function(x, keep.rownames=FALSE){
    class(x) <- setdiff(class(x), "whos")
    x
}
#' @param x \code{\link{whos}} object.
#' @param optional Ignored, kept for S3 consistency.
#' @param row.names Ignored, kept for S3 consistency.
#' @param ... Ignored, kept for S3 consistency.
#' @noRd
#' @export
as.data.frame.whos <- function(x, row.names=NULL, optional=FALSE, ...){
    class(x) <- setdiff(class(x), c("whos", "data.table"))
    x
}

