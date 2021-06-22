#'multi.nb
#'@export
multi.nb <- function(x, ...) {
    UseMethod("multi.nb", x)
}

#'multi.nb.numeric
#'@import multiUnion multiUnion
#'@method multi.nb numeric
#'@export
multi.nb.numeric <- function(x, nb) {
    out <- do.call(multiUnion::multiUnion, lapply(x, function(y) setdiff(nb[[y]], 
        x)))
    # print(out)
    out
}

#'multi.nb.integer
#'@import multiUnion multiUnion
#'@method multi.nb integer
#'@export
multi.nb.integer <- function(x, nb) {
    out <- do.call(multiUnion::multiUnion, lapply(x, function(y) setdiff(nb[[y]], 
        x)))
    # print(out)
    out
}

#'multi.nb.ultraCombo
#'@import ultraCombo revCombnGG union.combo
#'@method multi.nb ultraCombo
#'@export
multi.nb.ultraCombo <- function(x, nb) {
    revCombnGen <- ultraCombo::revCombnGG(length(nb))
    do.call(ultraCombo::union.combo, lapply(seq(x$len), function(y) {
        z1 <- x$Gen(y)
        z2 <- multi.nb(z1, nb)
        revCombnGen(do.call(rbind, lapply(z2, c, z1)))
    }))
}
