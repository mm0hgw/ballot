
library(buildPackage)
library(ballot)

borghesiDir <- "test/pics/borghesi"

sbSampleTable <- function(sb) {
    sample <- sbCalculateSample(sb)
    out <- cbind(sb, sample = sample)
    out[order(sample, decreasing = TRUE), ]
}

system(paste("mkdir -p", borghesiDir))
SIR_turnout <- dget("test/data/Scotland2")[4:6]

plotSbList <- function(sbList, fileOffset = 0, tag = NULL) {
    lapply(seq_along(sbList), function(i) {
        filePrefix <- paste(sep = "", borghesiDir, "/borghesi", i + fileOffset)
        pngname <- paste(sep = "", filePrefix, ".png")
        csvname <- paste(sep = "", filePrefix, ".csv")
        sb <- sbList[[i]]
        d <- sbDensity(sb)
        dy <- dnorm(d$x, mean = sbPopMean(sb), sd = sbPopSd(sb))
        png(filename = pngname, height = pngHeight, width = pngWidth)
        plot(d, main = names(sbList)[i])
        lines(d$x, dy, lty = 2)
        dev.off()
        write.csv(file = csvname, sbSampleTable(sb))
        # gitAdd(print(c(pngname,csvname)))
        if (!is.null(tag)) {
            spname <- paste(sep = "", borghesiDir, "/borghesi-spatial", i + fileOffset, 
                testSuffix)
            png(filename = spname, height = pngHeight, width = pngWidth)
            spatialPlot(tag, sbCalculateSample(sb, norm = TRUE))
            dev.off()
            # gitAdd(print(spname))
        }
    })
}

load("test/data/elections.RData")
SIR <- elections$SIR2014[, -1]
colnames(SIR)[1] <- "N"
sbList <- c(SIR_turnout, splitBallot(SIR))
plotSbList(SIR_turnout, tag = ls.ballotTag("SIR"))
fileOffset <- length(SIR_turnout)
plotSbList(splitBallot(SIR), fileOffset, tag = ls.ballotTag("SIR"))

