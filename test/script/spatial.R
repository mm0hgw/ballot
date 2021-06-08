
system("mkdir -p test/pics/spatial")

lapply(ls.ballotTag(), function(ballotTagName) {
    x <- as.ballotTag(ballotTagName)
    cat("spatial plot", x, "\n")
    sbList <- splitBallot(get.ballot(x))
    sbList <- sbList[sapply(sbList, sbSum) != 0]
    lapply(seq_along(sbList), function(i) {
        name <- names(sbList)[i]
        sb <- sbList[[i]]
        if (name == "V") {
            name <- "Abstainers"
            sb <- sbAbstainers(sb)
        }
        testFile <- paste(sep = "", "test/pics/spatial/", gsub(" ", ".", name), "_ballotTag_test_", 
            ballotTagName)
        a <- strsplit(x, "\\.")[[1]]
        year <- a[length(a) - 1]
        title <- paste(sep = ", ", name, year)
        if ("pdf" == outputType) 
            pdf(paste0(testFile, ".pdf"), paper = pdfPaper)
        if ("png" == outputType) 
            png(paste0(testFile, ".png"), height = pngHeight, width = pngWidth)
        spatialPlot(x, sample = sbCalculateSample(sb, norm = TRUE), main = title)
        dev.off()
    })
})
