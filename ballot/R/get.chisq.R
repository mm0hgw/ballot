
#'get.chisq
#'@param x ballotTag
#'@param party a 'character' index
#'@export get.chisq
get.chisq <- function(x, party = "V") {
    stopifnot(length(x) == 1)
    UseMethod("get.chisq", x)
}

#'@method get.chisq character
get.chisq.character <- function(x, party = "V") {
    x1 <- strsplit(x, " ")[[1]]
    x2 <- as.ballotTag(x1[1])
    if (length(x1 > 1)) {
        party <- x1[2]
    }
    return(get.chisq(x2, party))
}

#'@method get.chisq ballotTag
get.chisq.ballotTag <- function(x, party = "V") {
    dataName <- paste(sep = ".", x, gsub(" ", ".", party), "chisq")
    dataFile <- paste(sep = "", ballotDir, dataName, ".rda")
    tmpEnv <- new.env()
    if (file.exists(dataFile)) {
        load(dataFile, envir = tmpEnv)
    } else {
        LAPPLYFUN <- get.lapply::get.lapply()
        chunkSize <- get.lapply::get.chunkSize()
        z.combo <- get.combo(x)
        if (party %in% c("!V", "Abstainers")) {
            z.sb <- sbAbstainers(splitBallot(get.ballot(x))[["V"]])
        } else {
            z.sb <- splitBallot(get.ballot(x))[[party]]
        }
        stopifnot(!is.null(z.sb))
        chisq <- do.call(c, LAPPLYFUN(ultraCombo::comboChunk(z.combo, by = chunkSize), 
            function(combo) {
                dc <- ultraCombo::dataCombo(combo, z.sb, sbChisqTest)
                sapply(seq(dc$len), dc$dGen)
            }))
        assign(dataName, chisq, envir = tmpEnv)
        xzSave(list = ls(tmpEnv), file = dataFile, envir = tmpEnv)
    }
    return(get(dataName, envir = tmpEnv))
}
