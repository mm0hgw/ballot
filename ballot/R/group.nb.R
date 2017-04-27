#'group.nb
#'@description Given the context of an 'nb' object and a
#'subset of occupied territory, return the territory outwith
#'the input subset that borders the input subset.
#'@param nb 'nb' object
#'@param subset a 'vector' defining the currently occupied subset.
#'@importFrom sp plot
#'@export
group.nb <- function(nb, subset) {
    stopifnot(valid.nb.subset(nb, subset))
    if (is.logical(subset)) 
        subset <- seq(length(nb))[subset]
    class(nb) <- "list"
    v1 <- unique(do.call(c, nb[subset]))
    v2 <- setdiff(v1, c(0, subset))
    sort(v2)
}

#'group.nb.test
#'@param x a fooTag
#'@param subset a subset of fooTag
#'@export
group.nb.test <- function(x, subset) {
    sp <- get.Spatial(x)
    nb <- group.nb(x)
    stopifnot(valid.nb.subset(nb, subset))
    if (is.logical(subset)) 
        subset <- seq(length(nb))[subset]
    n <- length(nb)
    plot(sp)
    plot(sp[subset, ], border = NA, col = 1, add = TRUE)
    col <- 2
    while (length(subset) < n) {
        invade <- group.nb(nb, subset)
        plot(sp[invade, ], border = NA, col = col, add = TRUE)
        subset <- union(subset, invade)
        col <- col + 1
    }
}
