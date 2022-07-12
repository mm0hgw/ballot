#'nb.add
#'@param nb an 'nb' object
#'@param links a 'list' of links
#'@importFrom spdep sym.attr.nb
#'@export
nb.add <- function(nb, links) {
    lapply(links, function(x) {
        a <- x[1]
        b <- x[2]
        nb[[a]] <<- as.integer(sort(union(setdiff(nb[[a]], 0), b)))
        nb[[b]] <<- as.integer(sort(union(setdiff(nb[[b]], 0), a)))
    })
    sym.attr.nb(nb)
}

#'nb.del
#'@export
#'@importFrom spdep sym.attr.nb
#'@inheritParams nb.add
nb.del <- function(nb, links) {
    lapply(seq(length(nb)), function(x) {
        a <- x[1]
        b <- x[2]
        nb[[a]] <<- sort(setdiff(nb[[a]], b))
        nb[[b]] <<- sort(setdiff(nb[[b]], a))
        if (length(nb[[a]]) == 0)
            nb[[a]] <<- 0L
        if (length(nb[[b]]) == 0)
            nb[[b]] <<- 0L
    })
    sym.attr.nb(nb)
}
