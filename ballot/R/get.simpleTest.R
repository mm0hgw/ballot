
#'get.simpleTest
#'@param x ballotTag
#'@param party a 'character' index
#'@export get.simpleTest
get.simpleTest <- function(x, ...) {
    stopifnot(length(x) == 1)
    UseMethod("get.simpleTest", x)
}

#'get.simpleTest.character
#'@method get.simpleTest character
#'@export
get.simpleTest.character <- function(x, ...) {
    x <- as.ballotTag(x)
    NextMethod()
}

#'get.simpleTest.ballotTag
#'@method get.simpleTest ballotTag
#'@importFrom ultraCombo dataCombo
#'@importFrom getLapply forkBombGen
#'@export
get.simpleTest.ballotTag <- function(x, grepFilter = NULL, ...) {
    x.combo <- get.combo(x)
    x.b <- get.ballot(x)
    x.parties <- colnames(x.b)[-1]
    x.parties <- x.parties[colSums(x.b[, -1]) != 0]
    if ("V" %in% x.parties) 
        x.parties[x.parties == "V"] <- "Abstainers"
    if (!is.null(grepFilter)) 
        x.parties <- grep(grepFilter, x.parties, value = TRUE)
    print(x.parties)
    x.simpleTest <- lapply(x.parties, function(party) {
        cat(paste0("Processing ", party))
        dataName <- paste(sep = ".", x, party, "simpleTest")
        dataFile <- paste(sep = "", ballotDir, dataName, ".rda")
        tmpEnv <- new.env()
        if (file.exists(dataFile)) {
            cat(" ... found in cache\n")
            load(dataFile, envir = tmpEnv)
        } else {
            cat(" ... not fouud, building")
            partyTest <- reportAndCollate(x, party)
            assign(dataName, partyTest, envir = tmpEnv)
            xzSave(list = ls(tmpEnv), file = dataFile, envir = tmpEnv)
            cat(" ... built\n")
        }
        get(dataName, envir = tmpEnv)
    })
    names(x.simpleTest) <- x.parties
    invisible(x.simpleTest)
}
