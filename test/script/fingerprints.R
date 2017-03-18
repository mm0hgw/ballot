system('mkdir -p test/pics/fingerprints/')
GE2010 <- ls.ballotTag('Scotland.2010')
SIR2014 <- ls.ballotTag('Scotland.2014')
GE2015 <- ls.ballotTag('Scotland.2015')

testFile <- paste('test/pics/fingerprints/2010_SNP.png')
testPng(testFile)


retardPlot <- function(
	sb,
	norm=FALSE
){
	sample <- sbCalculateSample(sb,norm)
	lines(sbDensity(sb,norm))
	abline(v=sample)
}

