
library(buildPackage)
library(ballot)

borghesiDir <-'test/pics/borghesi'

system(paste('mkdir -p',borghesiDir))
SIR_turnout <- dget('test/data/Scotland2')[4:6]

plotSbList <-function(sbList,fileOffset=0,tag=NULL){
lapply(seq_along(sbList),
function(i){
filename <- paste(sep='',borghesiDir,'/borghesi',i+fileOffset,testSuffix)
sb <- sbList[[i]]
d <- sbDensity(sb)
dy <- dnorm(d$x,mean=sbPopMean(sb),sd=sbPopSd(sb))
testFun(filename)
plot(d,main=names(sbList)[i])
lines(d$x,dy,lty=2)
dev.off()
gitAdd(print(filename))
if(!is.null(tag)){
filename <- paste(sep='',borghesiDir,'/borghesi-spatial',i+fileOffset,testSuffix)
png(filename)
spatialPlot(tag,sbCalculateSample(sb,norm=TRUE))
dev.off()
gitAdd(print(filename))
}
})
}

load('test/data/elections.RData')
SIR <- elections$SIR2014[,-1]
colnames(SIR)[1]<-'N'
sbList <- c(SIR_turnout,splitBallot(SIR))
plotSbList(SIR_turnout,tag=ls.ballotTag('SIR'))
fileOffset<-length(SIR_turnout)
plotSbList(splitBallot(SIR),fileOffset,tag=ls.ballotTag('SIR'))

