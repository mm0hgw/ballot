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
    if (!("xlim" %in% names(arg))) 
        arg$xlim <- c(0, 1)
    if (!("ylim" %in% names(arg))) {
        arg$ylim <- do.call(range, lapply(fList, "[", "freq"))
    }
    print(arg)
    do.call(sp::plot, arg)
    len <- length(fList)
    col <- seq(len) + 1
    lapply(seq(len), function(x) lines(x = fList$x/7, y = fList$freq/(get.combo(x)$len), 
        col = col[x], pch = col[x], lwd = 3))
    leg <- gsub("^V$", "Overall Turnout", names(fList))
    legend("topright", legend = leg, col = col, pch = col)
}
