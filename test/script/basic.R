system("mkdir -p test/pics/layers/")

lapply(ls.layerTag(), function(layerTagName) {
    cat("layer plot", layerTagName, "\n")
    testFile <- paste(sep = "", "test/pics/layers/", layerTagName, "_layerTag_test")
    if ("pdf" == outputType) 
        pdf(paste0(testFile, ".pdf"), paper = pdfPaper)
    if ("png" == outputType) 
        png(paste0(testFile, ".png"), height = pngHeight, width = pngWidth)
print(testFile)
print(layerTagName)
    plot(as.layerTag(layerTagName))
    dev.off()
#    if (buildPackageLoaded) 
#        gitAdd(print(testFile))
})

system("mkdir -p test/pics/regions/")

lapply(ls.regionTag(), function(regionTagName) {
    cat("region plot", regionTagName, "\n")
    testFile <- paste(sep = "", "test/pics/regions/", regionTagName, "_regionTag_test")
    if ("pdf" == outputType) 
        pdf(paste0(testFile, ".pdf"), paper = pdfPaper)
    if ("png" == outputType) 
        png(paste0(testFile, ".png"), height = pngHeight, width = pngWidth)
#    testFun(testFile)
    plot(regionTag(regionTagName))
    dev.off()
    if (buildPackageLoaded) 
        gitAdd(print(testFile))
})

system("mkdir -p test/pics/ballot/")

lapply(ls.ballotTag(), function(ballotTagName) {
    x <- as.ballotTag(ballotTagName)
    cat("ballot plot", x, "\n")
    testFile <- paste(sep = "", "test/pics/ballot/", ballotTagName, "_ballotTag_sample")
    a <- strsplit(x, "\\.")[[1]]
    year <- a[length(a) - 1]
    if ("pdf" == outputType) 
        pdf(paste0(testFile, ".pdf"), paper = pdfPaper)
    if ("png" == outputType) 
        png(paste0(testFile, ".png"), height = pngHeight, width = pngWidth)
#    testFun(testFile)
    plot(x, norm = FALSE)
    dev.off()
#    if (buildPackageLoaded) 
#        gitAdd(print(testFile))
    testFile <- paste(sep = "", "test/pics/ballot/", ballotTagName, "_ballotTag_norm")
    a <- strsplit(x, "\\.")[[1]]
    year <- a[length(a) - 1]
    if ("pdf" == outputType) 
        pdf(paste0(testFile, ".pdf"), paper = pdfPaper)
    if ("png" == outputType) 
        png(paste0(testFile, ".png"), height = pngHeight, width = pngWidth)
#    testFun(testFile)
    plot(x, norm = TRUE)
    dev.off()
    if (buildPackageLoaded) 
        gitAdd(print(testFile))

    testFile <- paste(sep = "", "test/pics/ballot/", ballotTagName, "_ballotTag.csv")
    write.csv(get.ballot(x), file = testFile)
    if (buildPackageLoaded) 
        gitAdd(print(testFile))
})
