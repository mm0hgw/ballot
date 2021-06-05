buildPackageLoaded <- requireNamespace('buildPackage')

if(buildPackageLoaded)library(buildPackage)
if(buildPackageLoaded)buildPackage("ballot",build=1)
library(ballot)

testSuffix <- '.png'
rawTestFun <- png
testFnParameters <- c(width=360,height=640)

testFun <- function(...){
	parameters <- as.list(c(...,testFnParameters))
	print(parameters)
	do.call(rawTestFun,parameters)
}

require(mclapplyFunGen)

system("mkdir -p test/pics/")

do.import <- function(){
	source('test/script/fuzzymatch.R')
	source('test/script/layerTag_import.R')
	source('test/script/regionTag_import.R')
	source('test/script/SIR2014_import.R')
	source('test/script/UK_GE_2010_import.R')
}

if(length(ls.ballotTag())==0)
	do.import()

#source('test/script/basic.R')
source('test/script/borghesi.plots.R')
#source('test/script/spatial.R')
#source('test/script/fingerprints.R')
#source('test/script/fingerprints2.R')
#source('test/script/fingerprints3.R')
#source('test/script/dataCombo.R')
#source('test/script/dataCombo2.R')
#source('test/script/freq.R')

if(buildPackageLoaded)buildPackage::gitPush("test")
