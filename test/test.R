# buildPackageLoaded <- requireNamespace('buildPackage')

# if (buildPackageLoaded) library(buildPackage)
# if(buildPackageLoaded)buildPackage('ballot',build=1)
library(ballot)

if (!exists('outputType')) outputType <- "png"
if (!exists('pngHeight')) pngHeight <- 1680
if (!exists('pngWidth')) pngWidth <- 720
if (!exists('pdfPaper')) pdfPaper <- "a4"

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

do.basicTestPlot <- function() {
    source("test/script/basic.R")
}

do.indyrefPlots <- function() {
    source("test/script/borghesi.plots.R")
    source("test/script/fingerprints.R")
}

do.SpatialPlots <- function() {
    source("test/script/spatial.R")
}

do.dataCombo <- function(){
 source('test/script/dataCombo.R')
}

# source('test/script/fingerprints2.R') source('test/script/fingerprints3.R')
# source('test/script/dataCombo2.R')
# source('test/script/freq.R')

