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
        printf("%s:\n", style(if(is.blank(names(x)[i])) i else names(x)[i], font="bold"))
        obj <- x[[i]]
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
