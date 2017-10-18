#'get.freq
#'@param x a 'ballotTag'
#'@export
get.freq <- function(x) {
    cat(paste("get.freq", x, "\n"))
    x <- as.ballotTag(x)
    ballot <- get.ballot(x)
    dataName <- paste(sep = "", x, "_freq")
    fileName <- paste(sep = "", ballotDir, dataName, ".rda")
    print(fileName)
    if (file.exists(fileName)) {
        tmpEnv <- new.env()
        load(fileName, envir = tmpEnv)
        get(dataName, envir = tmpEnv)
    } else {
        tmpEnv <- new.env()
		    sbList <- head(splitBallot(ballot), n=sum(colSums(ballot!=0)>nrow(ballot)/2)-1)
		    freq <- lapply(lapply(sbList, reportAndCollate, x=x, sbPointsBelowPopMean, c),plyr::count)
        print(freq)
        assign(dataName, freq, envir = tmpEnv)
        save(list = dataName, file = fileName, envir = tmpEnv)
        get(dataName, envir = tmpEnv)
    }
}
