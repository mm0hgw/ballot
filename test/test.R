buildPackageLoaded <- requireNamespace('buildPackage')

if(buildPackageLoaded)library(buildPackage)
if(buildPackageLoaded)buildPackage("ballot",build=1)

testPng <- function(...){
	png(...,width=1000,height=1000)
}

get.lapply::set.lapply(mclapplyFunGen::mclapplyFunGen())

system("mkdir -p test/pics/")

source('test/script/fuzzymatch.R')
source('test/script/layerTag_import.R')
source('test/script/regionTag_import.R')
source('test/script/SIR2014_import.R')
source('test/script/UK_GE_2010_import.R')
source('test/script/test.R')

if(buildPackageLoaded)buildPackage::gitPush("test")
