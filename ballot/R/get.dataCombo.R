#'get.dataCombo
#'@param x a 'ballotTag' object
#'@param FUN a 'function' or 'primitive' for the dataCombo. Default is
#'return()
#'@param key a 'vector' to key ballot columns. Default is TRUE
#'@importFrom ultraCombo dataCombo
#'@export
get.dataCombo <- function(x, FUN = return, key = TRUE) {
    stopifnot("ballotTag" %in% class(x))
    stopifnot(is.function(FUN) || is.primitive(FUN))
    combo <- get.combo(x)
    ballot <- get.ballot(x)[, key]
    dataCombo(combo, ballot, FUN)
}
