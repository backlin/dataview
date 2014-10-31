print.ls_str <- function (x, max.level = 1, give.attr = FALSE, ...,
                          digits = max(1, getOption("str")$digits.d)){
    E <- attr(x, "envir")
    stopifnot(is.environment(E))
    M <- attr(x, "mode")
    args <- list(...)
    if (length(args) && "digits.d" %in% names(args)) {
        if (missing(digits)) 
            digits <- args$digits.d
        else warning("'digits' and 'digits.d' are both specified and the latter is not used")
        args$digits.d <- NULL
    }
    strargs <- c(list(max.level = max.level, give.attr = give.attr, 
        digits = digits), args)
    for (nam in x) {
        cat(nam, ": ")
        o <- tryCatch(get(nam, envir = E, mode = M), error = function(e) e)
        if (inherits(o, "error")) {
            cat(if (length(grep("missing|not found", o$message))) 
                "<missing>"
            else o$message, "\n", sep = "")
        }
        else {
            strO <- function(...) str(o, ...)
            do.call(strO, strargs, quote = is.call(o) || is.symbol(o))
        }
    }
    invisible(x)
}
whos <- function(){
    out <- print(ls
print.ls_str


#' Display contents of an evironment, data.frame or list as a summary table
#'
#' Color coded according to class and dimensions of contents. See
#' \code{\link[xtermStyle]{style}} for details.
#'
#' @param pattern Regexp filtering of objects. Only objects matching the pattern
#'   are displayed. Optional, default: show all objects.
#' @param envir Environment, data frame or list to be displayed. Optional,
#'   default: globalenv()
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
#' @export
whos <- function(pattern="", envir=as.environment(-1), exclude=getOption("whos.mask")){
    # Check if the user specified a pattern, environment or both
    if(!is.character(pattern) && missing(envir)) {
        envir <- pattern
        pattern <- ""
    }
    # Get names of objects in environment matching pattern
    if(is.environment(envir)){
        obj.name <- ls(envir=envir, pattern=pattern)
    } else if(isS4(envir)){
        obj.name <- grep(pattern, slotNames(envir), value=TRUE)
    } else {
        obj.name <- grep(pattern, if(!is.null(names(envir))) names(envir) else 1:length(envir), value=TRUE)
    }
    obj.name <- obj.name[!obj.name %in% exclude]
    # Present objects
    n <- length(obj.name)
    if(n == 0){
        cat("No objects found\n")
    } else {
        # Make an object/property matrix (objects as rows, properties as columns)
        obj <- matrix("", n, 7, dimnames=list(NULL, c("name", "class", "S4", "dim", "bytes", "bytes.prefix", "comment")))
        # Go through all objects and assess their properties
        obj[,"name"] <- obj.name
        total.size <- 0
        for(i in 1:n){
            o <- objfun(envir, obj.name[i])
            obj[i,"class"] <- if(length(class(o)) > 1){
                paste(class(o)[1], "...")
            } else if(class(o) == "factor"){
                sprintf("factor (%i)", length(levels(o)))
            } else class(o)
            if(obj[i,"class"] %in% c("matrix", "array")) obj[i,"class"] <- paste(mode(o), obj[i,"class"])
            obj[i,"S4"] <- if(isS4(o)) "S4  " else ""
            obj[i,"dim"] <- dimfun(o)
            obj[i, c("bytes", "bytes.prefix")] <- sizefun(o)
            total.size <- total.size + object.size(o)
            obj[i,"comment"] <- if(!is.blank(attr(o, "comment"))) "?!  " else ""  # comment() can be slow for large objects, use attr() instead
        }
        if(any(obj[,"S4"] != "")) obj[obj[,"S4"] == "","S4"] <- "    "
        if(any(obj[,"comment"] != "")) obj[obj[,"comment"] == "","comment"] <- "    "
        # Determine how many characters each column occupies i.e. length of longest entry in each column
        nc <- apply(obj, 2, function(x) max(nchar(x)))
        # Output table
        for(i in 1:n){
            cat(style.dim(sprintf(paste("%", ceiling(log10(n+1)), "i", sep=""), i)),
                style.auto(objfun(envir, obj.name[i]),
                    sprintf(paste("  %-",nc["name"],"s  %-",nc["class"],"s  %s%-",nc["dim"],"s %6.1f %-3s  %s", sep=""),
                        obj[i,"name"], obj[i,"class"], obj[i,"S4"], obj[i,"dim"], as.numeric(obj[i,"bytes"]), obj[i,"bytes.prefix"], obj[i,"comment"])),
                "\n", sep="")
        }
        # Output total size of all objects in table (not nessecarily all objects in workspace)
        i <- trunc(log(total.size) / log(1024))
        total.size <- exp(log(total.size) - i*log(1024))
        total.size.suffix <- c("B", "KiB", "MiB", "GiB", "TiB")[i+1]
        cat(style.dim(paste(c(rep(" ", ceiling(log10(n+1)) + 2*4+sum(nc[1:4]) - 7),
                          sprintf("Total %6.1f %-3s  \n", total.size, total.size.suffix)),
                          collapse="")))
    }
}

#' Set a default exclusion mask for \code{\link{whos}}.
#'
#' @param lst List of object names. These will be hidden from view. Defaults to
#'   all objects in \code{envir}.
#' @param envir Environment to work in.
#' @return Nothing. The mask is stored as `whos.mask' in the global option list.
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
whos.set.mask <- function(lst, envir=globalenv()){
    # This function sets the default exclusion mask for the `whos' function.
    # It is stored as a list in the global options. When "whosing" objects fully
    # matching an entry in the mask will be omitted from the output.
    if(missing(lst)) options(whos.mask = ls(envir=envir))
    else options(whos.mask = lst)
}

#' Shortcut for calling whos without exclusion.
#'
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

#' Display vectors, lists or rows of a data frames in key-value-pairs.
#'
#' Color coded according to class of contents.
#'
#' @param x List or data frame.
#' @param i Row number to show. Press down/up to browse.
#' @param sort.fields Display the elements in alphabetical order of the names.
#'   Optional, default: FALSE.
#' @param fmt \code{\link{sprintf}} type formatting string which will be
#'   applied to numbers, e.g. for specifying number of decimals or alignment.
#' @return Nothing.
#' @examples
#' entry.view(Sys.getenv())
#' entry.view(rnorm(20), fmt="\%5.2f")
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
entry.view <- function(x, i=1, sort.fields=FALSE, fmt=NULL){
    df.input <- class(x) == "data.frame"
    if(df.input) df <- x
    
    browsing <- TRUE
    while(browsing){
    
        if(df.input){
            if(nrow(df) > 1) cat("Showing row", i, "of", nrow(df))
            if(rownames(df)[i] != as.character(i)) cat(":", rownames(df)[i])
            cat("\n")
            x <- as.list(df[i,])
        }

        terminal.width <- if(is.blank(Sys.getenv("COLUMNS"))) 80L else as.integer(Sys.getenv("COLUMNS"))
        name.maxlength <- round(.6*terminal.width)
        my.names <- if(is.blank(names(x))) as.character(1:length(x)) else names(x)
        my.names <- sapply(my.names, function(n){
            if(nchar(n) > name.maxlength){
                sub(paste("^(.{0,", name.maxlength, "}).*$", sep=""), "\\1...", n)
            } else {
                n
            }
        })
        name.maxlength <- max(sapply(my.names, nchar))
        output.width <- terminal.width - 4 - name.maxlength
        for(j in (if(sort.fields) order(my.names) else 1:length(x))){
            # objfun should be used here
            obj <- objfun(x, j)
            content.str <- if(mode(obj) %in% c("numeric", "logical", "character", "factor")){
                    str <- paste(obj, collapse=", ")
                    if(nchar(str) > output.width){
                        sprintf("%s %s", mode(obj), dimfun(obj))
                    } else {
                        if(is.numeric(obj) && length(obj) == 1 && !is.blank(fmt)){
                            sprintf(fmt, obj)
                        } else str
                    }
                } else {
                    if(mode(obj) != class(obj)){
                        sprintf("%s %s", mode(obj), class(obj))
                    } else
                        class(obj)
                }
            
            cat(rep(" ", 2 + name.maxlength - nchar(my.names[j])),
                style.dim(sprintf("%s: ", my.names[j])),
                style.auto(obj, content.str),
                "\n", sep="")
        }
        if(df.input){
            chars <- c(`85`="U", `117`="u", `100`="d", `68`="D", `103`="g")
            new.i <- i
            while(new.i == i){
                if(browse == 2){
                    inchar <- chars[as.character(.Call("get_char", PACKAGE="dataview"))]
                    cat("\n\n")
                } else {
                    inchar <- readline("Browse with u,d,U,D,g: ")
                }
                
                if(!inchar %in% chars){
                    browsing <- FALSE
                    new.i <- 0
                } else {
                    if(inchar == "g"){
                        new.i <- as.integer(readline("Enter line number: "))
                    } else {
                        new.i <- i + c(-10, -1, 1, 10)[which(inchar == chars)]
                    }
                    if(new.i < 1){
                        if(i == 1) cat("Already at the top\n")
                        new.i <- 1
                    }
                    if(new.i > nrow(df)){
                        if(i == nrow(df)) cat("Already at the bottom\n")
                        new.i <- nrow(df)
                    }
                }
            }
            i <- new.i
        } else {
            browsing <- FALSE
        }
    } # while browsing
}


#' Display heatmaps and heatvectors.
#'
#' Quickly see the overall pattern of a variable in the terminal.
#'
#' @param x Vector to be displayed.
#' @param pal Palette. Either the name of a palette defined in \code{\link[xtermStyle]{xterm.pal}}
#'   or an integer vector with color indices (see \code{\link[xtermStyle]{display.xterm.colors}}).
#' @param rng The numerical range which the palette describes. See \code{\link[xtermStyle]{discrete.color}}
#'   for more info.
#' @param width Length of each line. Optional.
#' @return Nothing
#' @examples
#' data(iris)
#' heat.view(iris$Species)
#' heat.view(matrix(iris$Petal.Width, 3, 50, byrow=TRUE, dimnames=list(levels(iris$Species), NULL)), pal="purples")
#'
#' run.status <- factor(runif(100) < .95, labels=c("Fail", "Pass"))
#' heat.view(run.status, pal=1:2)
#'
#' #Tip for displayig the element names of a named vector:
#' a <- runif(7)
#' names(a) <- c("ATM", "CHK1", "CDC25", "p53", "CDC2", "CDK2", "CDK4")
#' heat.view(a)            # No names displayed
#' heat.view(as.matrix(a)) # Names displayed
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
heat.view <- function(x, pal, rng, width){
    if(is.data.frame(x)){
        x <- sapply(x, function(x){
            if(is.character(x))
                return(rep(NA, length(x)))
            if(is.factor(x))
                return(as.integer(x))
            return(x)
        })
    }
  
    dark.bg <- is.null(options()$color.scheme) || options()$color.scheme != "light on dark"
    if(!missing(pal)){
        if(is.character(pal)) pal <- xterm.pal(pal)
        if(is.blank(pal)) stop("Palette not found.")
        if(is.list(pal)) pal <- pal[[1]]
    }
    terminal.width <- if(is.blank(Sys.getenv("COLUMNS"))) 80L else as.integer(Sys.getenv("COLUMNS"))
    
    if(is.logical(x)){
        if(missing(pal)) pal <- c(12,11)
        col <- discrete.color(x, range=c(0,1), pal)
        legend.str <- c(style("False", fg=pal[1]), style("True", fg=pal[2]))
    } else if(is.numeric(x)){
        if(missing(rng)){
            range.domain <- sign(sum(sign( range(x, na.rm=TRUE) )))
            rng <- if(range.domain == 0){  # Crossing zero
                 c(-1, 1)*max(abs(x), na.rm=TRUE)
            } else {
                range(x, na.rm=TRUE)
            }
        } else {
            range.domain <- sign(sum(sign(rng)))
        }
        if(missing(pal)){
            pal <- switch(as.character(range.domain),
                "-1" = if(dark.bg) xterm.pal("Blues")[[1]] else rev(xterm.pal("Blues")[[1]]),
                 "0" = if(dark.bg) xterm.pal("DownUp")[[1]] else xterm.pal("long")[[1]],
                 "1" = if(dark.bg) rev(xterm.pal("YlOrRd")[[1]]) else xterm.pal("YlOrdRd")[[1]])
        }
        col <- discrete.color(x, rng, pal)
        legend.str <- rep("", length(pal)+2)
        legend.str[1] <- trim(sprintf("%6g", rng[1]))
        legend.str[length(pal)+2] <- trim(sprintf("%6g", rng[2]))
        for(i in 1:length(pal)) legend.str[i+1] <- style(
            if(abs(i - 1 - (length(pal)-1)/2) < 1) "x" else "#", fg=pal[i])
    } else if(is.factor(x)) {
        if(missing(pal)){
            if(is.ordered(x)){
                pal <- xterm.pal("long")[[1]]
                pal <- pal[round(seq(1,25, length=length(levels(x))))]
            } else {
                pal <- xterm.pal("Set3")[[1]]
                pal <- pal[(1:length(levels(x))-1) %% length(pal) + 1]
            }
        }
        legend.str <- rep("",length(levels(x)))
        for(i in 1:length(levels(x))) legend.str[i] <- style(sprintf("`%s`", levels(x)[i]), fg=pal[i])
        if(length(legend.str) > 10){
            legend.str <- c(legend.str[1:9], "...", legend.str[length(legend.str)])
        }
        col <- pal[as.integer(x)]
    } else
        stop("Datatype not yet supported.")

    if(is.blank(dim(x)) || length(dim(x)) == 1){
        n <- length(x)
        n.digits <- ceiling(log10(n+1))
        if(missing(width)){
            width <- terminal.width - n.digits - 2
            width <- floor(width / 10)*10
        }
        # Go through the rows
        for(i in 1:ceiling(n/width)){
            printf(sprintf("%%%ii  ", n.digits), (i-1)*width+1)
            prev.style <- -1
            for(j in ((i-1)*width+1):min(n, i*width)){
                # Only change style if the new element differs from last,
                # easier to parse
                if(is.na(col[j])){
                    if(prev.style != -1) cat(style.clear(make.default=FALSE))
                    prev.style <- -1
                    cat("-")
                } else {
                    if(col[j] != prev.style){
                        cat(style.set(bg=col[j], make.default=FALSE))
                        prev.style <- col[j]
                    }
                    cat(" ")
                }
            }
            printf("%s\n", style.get())
        }
    } else if(length(dim(x)) == 2) {
        n <- nrow(x)
        rnames <- if(is.blank(rownames(x))) 1:nrow(x) else rownames(x)
        n.chars <- max(sapply(rnames, nchar))
        chr <- if(ncol(x) > terminal.width/2 - n.chars-2) " " else "  "
        for(i in 1:nrow(x)){
            printf(sprintf("%%%is  ", n.chars), rnames[i])
            prev.style <- -1
            for(j in 1:ncol(x)){
                if(is.na(col[i,j])){
                    if(prev.style != -1) cat(style.clear(make.default=FALSE))
                    prev.style <- -1
                    cat("-")
                } else {
                    if(col[i,j] != prev.style){
                        cat(style.set(bg=col[i,j], make.default=FALSE))
                        prev.style <- col[i,j]
                    }
                    cat(" ")
                }
                cat(chr)
            }
            printf("%s\n", style.get())
        }
    } else
        stop("Datatype not yet supported.")

    if(sum(nchar(legend.str[c(1, length(legend.str))])) + 2*length(legend.str) + 4 > terminal.width){
        printf("\n   %s %s %s\n\n", legend.str[1],
               paste(legend.str[3:length(legend.str)-1], collapse=""),
               legend.str[length(legend.str)])
    } else {
        printf("\n   %s\n\n", paste(legend.str, collapse=" "))
    }
}


#' Display contents of a list of lists in a summarized tree structure.
#'
#' @param x List to be displayed.
#' @param compact Wheter to display the list tree in compact mode. Optional,
#'   default: 'auto' i.e. adapt to terminal height.
#' @param show.data Whether to show the contents of the list elements or just
#'   the structure. Optional, default: 'auto' i.e. adapt to terminal width.
#' @param traverse.all Whether to treat objects of custom classes as lists or
#'   not traverse them (except if they are the root of the tree).
#' @param lines Maximum number of lines to show.
#' @param depth Maximum number of levels to show.
#' @param indent Internal.
#' @return Nothing
#' @examples
#' # Create a tree structure of lists
#' make.list.tree <- function(boost=2) {
#'     n.children <- round(boost + rexp(1))
#'     if(n.children < 1){
#'         return(rep("data", 1+floor(5*runif(1))))
#'     } else {
#'         ll <- vector("list", n.children)
#'         names(ll) <- paste("node", 1:n.children)
#'         return(lapply(ll, function(x) make.list.tree(boost-1)))
#'     }
#' }
#'
#' # Visualize it!
#' tree.view(make.list.tree())
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
tree.view <- function(x, compact='auto', show.data='auto', traverse.all=FALSE, lines=Inf, depth=Inf, indent=0){
    if(lines <= 0){
        cat(style.dim("Line limit reached\n"))
        return(NULL)
    }
    terminal.lines <- if(is.blank(Sys.getenv("LINES"))) 24L else as.integer(Sys.getenv("LINES"))
    terminal.width <- if(is.blank(Sys.getenv("COLUMNS"))) 80L else as.integer(Sys.getenv("COLUMNS"))
    
    if(compact == 'auto'){
        count.lines <- function(xx){
            if(is.list(xx)){
                if(is.blank(xx)){
                    return(1)
                } else {
                    return(1 + sum(sapply(xx, count.lines)))
                }
            } else return(1)
        }
        compact <- count.lines(x) > terminal.lines
    }

    # Do not traverse the tree if 'x' is of a
    # custom class unless it is also the root
    if(class(x)[1] == "list" || is.list(x) && (indent == 0 || traverse.all)){
        if(is.blank(x)){
            my.names <- list()
        } else {
            my.names <- as.list(1:length(x))
            if(!is.null(names(x))){
                my.names <- lapply(my.names, function(i) if(is.blank(names(x)[i])) i else names(x)[i])
            }
        }
    } else if(isS4(x)){
        my.names <- slotNames(x)
    } else {
        my.names <- NULL
    }
    
    if(is.null(my.names)){
        # We're in a leaf, show data
        data.str <- NULL
        try(data.str <- paste(x, collapse=", "), silent=TRUE)
        if(!is.null(data.str) && (show.data == TRUE ||
                (show.data == 'auto' && 
                nchar(data.str) < terminal.width - indent))){
            if(is.null(x)){ cat(style.auto(NULL, "NULL"), "\n", sep="")
            } else cat(style.auto(x, data.str), "\n", sep="")
        } else {
            cat(style.auto(x, paste(class(x), dimfun(x, use.names=TRUE))), "\n")
        }
    } else {
        if(length(my.names) == 0){
            cat(style.auto(NULL, "Empty list\n"))
        } else {
            for(i in 1:length(my.names)){
                if(i == 1 && !compact) cat("\n")
                if(i > 1 || !compact) cat(rep(" ", indent), sep="")
                cat(style.auto(objfun(x, my.names[[i]]), my.names[[i]]),
                    style.dim(": "),
                    sep="")
                if(depth > 1){
                        tree.view(objfun(x, my.names[[i]]), compact, show.data,
                            traverse.all, lines-1, depth-1,
                            indent + 2 + compact * nchar(my.names[[i]]))
                } else {
                    cat(style.auto(NULL, "Max depth reached\n"))
                }
            }
        }
    }
}


#' Display contents of a vector or list as line wrapped text
#'
#' @param x Vector or list to be displayed.
#' @return Nothing
#' @examples
#' x <- rep(NA, 6)
#' for(i in 1:6) x[i] <- paste(c("m", "a", "r", "u", "l", "k", " ")[1+floor(7*runif(100+floor(500*runif(1))))], collapse="")
#' wrap.view(x)
#' 
#' x <- list(1:9, stuff=Sys.info(), today=date(), model=Outcome ~ Variables)
#' wrap.view(x)
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
wrap.view <- function(x){
    terminal.width <- if(is.blank(Sys.getenv("COLUMNS"))) 80L else as.integer(Sys.getenv("COLUMNS"))
    indent <- 4
    indent.str <- paste(rep(" ", indent), collapse="")
    for(i in 1:length(x)){
        printf("%s:\n", style(if(is.blank(names(x)[i])) i else names(x)[i], font.style="bold"))
        obj <- objfun(x, i)
        if(mode(obj) %in% c("numeric", "logical", "character", "factor")){
            obj <- paste(as.character(obj), collapse=", ")
            while(!is.blank(obj)){
                obj <- sub("^\\s*", "", obj)
                l <- regexpr(sprintf("^.{,%i}\\b", terminal.width-indent-1), obj)
                l <- if(attr(l, "match.length") != -1) attr(l, "match.length") else terminal.width - 4
                printf("%s%s\n", indent.str, substr(obj, 0, l))
                obj <- substr(obj, l+1, nchar(obj))
            }
            printf("    %s\n", obj)
        } else {
            printf("%s%s\n\n", indent.str, style.auto(obj,
                if(mode(obj) != class(obj)){
                    sprintf("%s <%s>", mode(obj), class(obj))
                } else
                    class(obj)))
        }
    }
}

