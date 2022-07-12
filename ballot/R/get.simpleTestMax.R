#'get.simpleTestMax
#'@export
get.simpleTestMax <- function(x, x.parties = NULL, n = 20, dcFUN = sbReportGen()) {
    if (!is.ballotTag(x))
        x <- as.ballotTag(x)
    x.ballot <- get.ballot(x)
    x.combo <- get.combo(x)
    x.parties <- get.parties(x, x.parties)
    x.simpleTest <- get.simpleTest(x, x.parties)
    out <- lapply(seq_along(x.parties), function(y) {
        y.party <- x.parties[y]
        if (y.party == "Abstainers") {
            y.ballot <- sbAbstainers(x.ballot[, c("N", "V")])
        } else {
            y.ballot <- x.ballot[, c("N", y.party)]
        }
        y.simpleTest <- x.simpleTest[[y]]
        y.naMask <- !is.na(y.simpleTest)
        if (sum(y.naMask) == 0)
            return(NA)
        y.simpleTest <- y.simpleTest[y.naMask]
        y.max <- head(n = n, order(y.simpleTest, decreasing = TRUE))
        y.combo <- x.combo[y.naMask][y.max]
        dataCombo(y.combo, y.ballot, dcFUN)
    })
    names(out) <- x.parties
    out
}
