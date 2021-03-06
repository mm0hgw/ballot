buildPackageLoaded <- requireNamespace("buildPackage")

if (buildPackageLoaded) library(buildPackage)
# if(buildPackageLoaded)buildPackage('ballot',build=1)
library(ballot)

outputType <- "png"
pngHeight <- 1680
pngWidth <- 720
pdfPaper <- "a4"

require(mclapplyFunGen)

system("mkdir -p test/pics/")

do.import <- function() {
    source("test/script/fuzzymatch.R")
    source("test/script/layerTag_import.R")
    source("test/script/regionTag_import.R")
    source("test/script/SIR2014_import.R")
    source("test/script/UK_GE_2010_import.R")
}

if (length(ls.ballotTag()) == 0) do.import()

source("test/script/basic.R")
source("test/script/borghesi.plots.R")
source("test/script/spatial.R")
source("test/script/fingerprints.R")
# source('test/script/fingerprints2.R') source('test/script/fingerprints3.R')
# source('test/script/dataCombo.R') source('test/script/dataCombo2.R')
# source('test/script/freq.R')

