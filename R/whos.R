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
#' data(iris)
#' whos()
#' whos.all()
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @import data.table
#' @import xtermStyle
#' @seealso whos.options, browse
#' @export
whos <- function(envir=parent.frame(), pattern=".", exclude=getOption("whos.exclude")){
    # Interpret the `envir` argument if not already an environment
    envir <- switch(class(envir)[1],
        `character` = {
            pattern <- envir
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

    if(length(obj.name) == 0){
        NULL
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
                if(is.function(x)){
                    ""
                } else {
                    x <- list(length = length(x), dim = dim(x))
                    if(is.null(x$dim)){
                        as.character(x$length)
                    } else {
                        paste(x$dim, collapse="x")
                    }
                }
            }),
            bytes = obj.sapply(if(getOption("whos.report.S4.size", TRUE)){
                function(x) if(isS4(x)) NA else object.size(x)
            } else {
                object.size
            }),
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
    size <- 2^(log2(x$bytes) %% 10)
    unit <- c("B", "KiB", "MiB", "GiB", "TiB", "EiB")[
        sapply(x$bytes, function(b) sum(b > 1024^(0:4)))]
    nc <- as.data.table(x)[, list(
        index = nchar(nrow(x)) + 1 + space,
        name = if(all(is.na(name))) 0 else max(nchar(name)) + space,
        table.key = if(any(table.key) %in% TRUE) 5 + space else 0,  # The %in% is a hack to get around any() --> NA
        class = max(nchar(class)) + space,
        S4 = if(any(S4) %in% TRUE) 4 + space else 0,
        dim = max(nchar(dim)) + space,
        size = 6,
        unit = max(nchar(unit)) + space,
        comment = if(any(comment) %in% TRUE) 2 + space
    )]
    sfun <- function(str, width) sprintf(sprintf("%%-%is", width), str)
    tryCatch({
        cat(as.data.table(x)[,paste0(
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
    }, interrupt = {
        cat(style.clear())
    })
}
#' @param x \code{\link{whos}} object.
#' @param ... Sent to 
#' @noRd
#' @export
`[.whos` <- function(x, ...){
    structure(as.data.table(x)[...], class=class(x))
}

#' Set default behavior of the whos function
#'
#' @param exclude Objects to exclude from view. Can be a character vector of
#'   names, or an environment, but not any regular expressions so far.
#' @param report.S4.size Calculating the size of S4 objects with
#'   \code{\link{object.size}} can take an annoyingly long time (seconds), set
#'   this option to \code{FALSE} to skip it and get quicker execution.
#' @return Nothing. The values are stored as global options.
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
whos.options <- function(exclude, report.S4.size){
    if(!missing(exclude)){
        options(whos.exclude = switch(class(exclude),
            `character` = exclude,
            `environment` = ls(exclude),
            `integer` = ls(as.environment(exclude))
        ))
    }
    if(!missing(report.S4.size))
        options(whos.report.S4.size = report.S4.size)
}

#' @param x A character vector of object names to exclude or include.
#' @param pattern \link[=regex]{Regular expression pattern} to match object
#'   names against, e.g. \code{pattern="^my\\..*"} will exclude or include
#'   \code{"my.vector"} and \code{"my.matrix"} but not \code{"mysql.con"}.
#' @param envir Environment to search in.
#' @rdname whos.options
#' @export
whos.exclude <- function(x=NULL, pattern, envir=parent.frame()){
    if(!missing(pattern)) x <- union(x, ls(envir=envir, pattern=pattern))
    options(whos.exclude = union(getOption("whos.exclude"), x))
}
#' @rdname whos.options
#' @export
whos.include <- function(x=NULL, pattern, envir=parent.frame()){
    if(!missing(pattern)) x <- union(x, ls(envir=envir, pattern=pattern))
    options(whos.exclude = setdiff(getOption("whos.exclude"), x))
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

#' Convert objects to whos
#'
#' @param x Object of type \code{\link{data.table}} or \code{\link{data.frame}}.
#' @examples
#' an.object <- "Containing all my stuff"
#' w <- as.data.frame(whos())
#' as.whos(w)
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
as.whos <- function(x){
    UseMethod("as.whos")
}
#' @rdname as.whos
#' @export
as.whos.data.table <- function(x){
    class(x) <- c("whos", class(x))
    x
}
#' @rdname as.whos
#' @export
as.whos.data.frame <- function(x){
    x <- as.data.table(x)
    as.whos(x)
}

