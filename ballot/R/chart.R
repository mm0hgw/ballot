#'freqPlot
#'@param x a 'ballotTag' object
#'@param ... extra graphical arguments
#'@export
freqPlot <- function(x, ...) {
    cat(paste("freqPlot", x, "\n"))
    arg <- list(...)
    if (!("main" %in% names(arg))) 
        arg$main <- paste("Frequency Plot,", get.bTitle(x))
    slice <- seq(3)
    if ("slice" %in% names(arg)) {
        slice <- arg$slice
        arg$slice <- NULL
    }
    if (!("xlab" %in% names(arg))) {
        arg$xlab <- "Proportion of counting areas"
    }
    if (!("ylab" %in% names(arg))) {
        arg$ylab <- "Proportion of combinations"
    }
    names(slice) <- colnames(get.ballot(x))[slice + 1]
    fList <- lapply(slice, get.freq, x = x)
    print(fList)
    f2List <- lapply(fList, function(i) cbind(x = i$x/7, y = i$freq/sum(i$freq)))
    if (!("xlim" %in% names(arg))) 
        arg$xlim <- c(0, 1)
    if (!("ylim" %in% names(arg))) {
        arg$ylim <- c(0, 1)
    }
    arg$x <- 0
    arg$type <- "n"
    print(f2List)
    print(arg)
    do.call(plot, arg)
    len <- length(f2List)
    col <- seq(len) + 1
    lapply(seq(len), function(z) lines(f2List[[z]], col = col[z], pch = col[z], lwd = 3))
    leg <- gsub("^V$", "Overall Turnout", names(fList))
    legend("topright", legend = leg, col = col, pch = col)
}
