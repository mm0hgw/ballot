
library(buildPackage)
library(ballot)

borghesiDir <-'test/pics/fingerprints/borghesi'

system(paste('mkdir -p',borghesiDir))
SIR_turnout <- dget('test/data/Scotland2')[4:6]

plotSbList <-function(sbList,fileOffset=0){
lapply(seq_along(sbList),
function(i){
filename <- paste(sep='',borghesiDir,'/borghesi',i+fileOffset,'.png')
sb <- sbList[[i]]
d <- sbDensity(sb)
dy <- dnorm(d$x,mean=sbPopMean(sb),sd=sbPopSd(sb))
png(filename)
plot(d,main=names(sbList)[i])
lines(d$x,dy,lty=2)
dev.off()
gitAdd(print(filename))
})
}

plotSbList(SIR_turnout)
fileOffset <- length (SIR_turnout)
load('test/data/elections.RData')
SIR <- elections$SIR2014[,-1]
colnames(SIR)[1]<-'N'
plotSbList(splitBallot(SIR),fileOffset)

