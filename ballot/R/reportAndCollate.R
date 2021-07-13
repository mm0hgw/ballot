abstainStrings <- c("!V", "Abstainers")

#'reportAndCollate
#'@param sb a 'subballot' object
#'@param SBREPORTFUN a 'function' that takes a sub-ballot matrix and returns a report
#'@param COLLATEFUN a 'function' that collates results
#'@importFrom getLapply forkBombGen
#'@importFrom ultraCombo dataCombo
#'@export
reportAndCollate <- function(x, key = "!V", SBREPORTFUN = sbSimpleTest, COLLATEFUN = c) {
    stopifnot(is.function(SBREPORTFUN) || is.primitive(SBREPORTFUN))
    stopifnot(is.function(COLLATEFUN) || is.primitive(COLLATEFUN))
    ballot <- get.ballot(x)
    stopifnot(length(key) == 1 && key %in% colnames(ballot) || key %in% abstainStrings)
    if (key %in% abstainStrings) 
        sb <- sbAbstainers(ballot[, c("N", "V")]) else sb <- ballot[, c("N", key)]
    dc <- dataCombo(get.combo(x), sb, SBREPORTFUN)
    dcForkBomb <- forkBombGen(dc$dGen, COLLATEFUN = COLLATEFUN)
    dcForkBomb(seq_along(dc))
}
