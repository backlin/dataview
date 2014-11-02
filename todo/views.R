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

