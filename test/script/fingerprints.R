system('mkdir -p test/pics/fingerprints/')
GE2010 <- splitBallot(get.ballot(ls.ballotTag('Scotland.2010')))
SIR2014 <- splitBallot(get.ballot(ls.ballotTag('Scotland.2014')))
GE2015 <- splitBallot(get.ballot(ls.ballotTag('Scotland.2015')))

sbList <- list(YeSNP2010=GE2010$SNP,
	YeSNP2014=SIR2014$Yes,
	YeSNP2015=GE2015$SNP
)
dList <- lapply(sbList,sbDensity,norm=FALSE)
arg <- list(x=0,
	type='n',
	xlim=ballot:::dListXlim(dList),
	ylim=ballot:::dListYlim(dList),
	main='YeSNP faction \'10-\'15',
	ylab='Density',
	xlab='Fractional Turnout'
)
col <- seq_along(dList)+1
testFile <- paste('test/pics/fingerprints/YeSNP.png')
testPng(testFile)
do.call(plot,arg)
lapply(seq_along(dList),
	function(i){
		lines(dList[[i]],col=col[i],lwd=2)
		x <- seq(min(dList[[i]]$x),max(dList[[i]]$x),length.out=256)
		lines(x,dnorm(x,mean=sbPopMean(sbList[[i]]),sd=sbPopSd(sbList[[i]])),lty=2)
	}
)
legend('topright',legend=names(sbList),col=col,lwd=2)
dev.off()
if(buildPackageLoaded)gitAdd(print(testFile))

dList <- lapply(sbList,sbDensity,norm=TRUE)
arg <- list(x=0,
	type='n',
	xlim=ballot:::dListXlim(dList),
	ylim=ballot:::dListYlim(dList),
	main='YeSNP faction \'10-\'15',
	ylab='Density',
	xlab='SDs from Population mean'
)
col <- seq_along(dList)+1
testFile <- paste('test/pics/fingerprints/YeSNP_norm.png')
testPng(testFile)
do.call(plot,arg)
x <- seq(arg$xlim[1],arg$xlim[2],length.out=256)
lines(x,dnorm(x),lty=2)
lapply(seq_along(dList),
	function(i){
		lines(dList[[i]],col=col[i],lwd=2)
	}
)
legend('topright',legend=names(sbList),col=col,lwd=2)
dev.off()
if(buildPackageLoaded)gitAdd(print(testFile))

sbListAbstain <- list(Abstainers.2010=sbAbstainers(GE2010$V),
Abstainers.2014=sbAbstainers(SIR2014$V),
Abstainers.2015=sbAbstainers(GE2015$V)
)
dList <- lapply(sbListAbstain,sbDensity,norm=FALSE)
arg <- list(x=0,
type='n',
xlim=ballot:::dListXlim(dList),
ylim=ballot:::dListYlim(dList),
main='Abstainers faction \'10-\'15',
ylab='Density',
xlab='Fractional Turnout'
)
col <- seq_along(dList)+1
testFile <- paste('test/pics/fingerprints/Abstainers.png')
testPng(testFile)
do.call(plot,arg)
lapply(seq_along(dList),
function(i){
lines(dList[[i]],col=col[i],lwd=2)
		x <- seq(min(dList[[i]]$x),max(dList[[i]]$x),length.out=256)
		lines(x,dnorm(x,mean=sbPopMean(sbList[[i]]),sd=sbPopSd(sbList[[i]])),lty=2)
}
)
legend('topright',legend=names(sbListAbstain),col=col,lwd=2)
dev.off()
if(buildPackageLoaded)gitAdd(print(testFile))
dList <- lapply(sbListAbstain,sbDensity,norm=TRUE)
arg <- list(x=0,
type='n',
xlim=ballot:::dListXlim(dList),
ylim=ballot:::dListYlim(dList),
main='Abstainers faction \'10-\'15',
ylab='Density',
xlab='SDs from Population mean'
)
col <- seq_along(dList)+1
testFile <- paste('test/pics/fingerprints/Abstainers_norm.png')
testPng(testFile)
do.call(plot,arg)
x <- seq(arg$xlim[1],arg$xlim[2],length.out=256)
lines(x,dnorm(x),lty=2)
lapply(seq_along(dList),
function(i){
lines(dList[[i]],col=col[i],lwd=2)
}
)
legend('topright',legend=names(sbListAbstain),col=col,lwd=2)
dev.off()
if(buildPackageLoaded)gitAdd(print(testFile))




GE2010U <- GE2010$V
GE2010U[,2] <- GE2010$V[,2] - GE2010$SNP[,2]
GE2015U <- GE2015$V
GE2015U[,2] <- GE2015$V[,2] - GE2015$SNP[,2]

sbListNo <- list( Unionist.2010=GE2010U,
Unionist.2014=SIR2014$No,
Unionist.2015=GE2015U
)
dList <- lapply(sbListNo,sbDensity,norm=FALSE)
arg <- list(x=0,
type='n',
xlim=ballot:::dListXlim(dList),
ylim=ballot:::dListYlim(dList),
main='Unionist faction \'10-\'15',
ylab='Density',
xlab='Fractional Turnout'
)
col <- seq_along(dList)+1
testFile <- paste('test/pics/fingerprints/Unionist.png')
testPng(testFile)
do.call(plot,arg)
lapply(seq_along(dList),
function(i){
lines(dList[[i]],col=col[i],lwd=2)
		x <- seq(min(dList[[i]]$x),max(dList[[i]]$x),length.out=256)
		lines(x,dnorm(x),lty=2)
		lines(x,dnorm(x,mean=sbPopMean(sbList[[i]]),sd=sbPopSd(sbList[[i]])),lty=2)
}
)
legend('topright',legend=names(sbListNo),col=col,lwd=2)
dev.off()
if(buildPackageLoaded)gitAdd(print(testFile))
dList <- lapply(sbListNo,sbDensity,norm=TRUE)
arg <- list(x=0,
type='n',
xlim=ballot:::dListXlim(dList),
ylim=ballot:::dListYlim(dList),
main='Unionist faction \'10-\'15',
ylab='Density',
xlab='SDs from Population mean'
)
col <- seq_along(dList)+1
testFile <- paste('test/pics/fingerprints/Unionist_norm.png')
testPng(testFile)
do.call(plot,arg)
x <- seq(arg$xlim[1],arg$xlim[2],length.out=256)
lines(x,dnorm(x),lty=2)
lapply(seq_along(dList),
	function(i){
		lines(dList[[i]],col=col[i],lwd=2)
	}
)
legend('topright',legend=names(sbListNo),col=col,lwd=2)
dev.off()
if(buildPackageLoaded)gitAdd(print(testFile))

sbListCon <- list(Conservative.2010=GE2010$Con,
	Conservative.2015=GE2015$Conservative
)

dList <- lapply(sbListCon,sbDensity,norm=FALSE)
arg <- list(x=0,
type='n',
xlim=ballot:::dListXlim(dList),
ylim=ballot:::dListYlim(dList),
main='Conservative faction \'10-\'15',
ylab='Density',
xlab='Fractional Turnout'
)
col <- seq_along(dList)+1
testFile <- paste('test/pics/fingerprints/Conservative.png')
testPng(testFile)
do.call(plot,arg)
lapply(seq_along(dList),
function(i){
lines(dList[[i]],col=col[i],lwd=2)
		x <- seq(min(dList[[i]]$x),max(dList[[i]]$x),length.out=256)
		lines(x,dnorm(x,mean=sbPopMean(sbList[[i]]),sd=sbPopSd(sbList[[i]])),lty=2)
}
)
legend('topright',legend=names(sbListCon),col=col,lwd=2)
dev.off()
if(buildPackageLoaded)gitAdd(print(testFile))
dList <- lapply(sbListCon,sbDensity,norm=TRUE)
arg <- list(x=0,
type='n',
xlim=ballot:::dListXlim(dList),
ylim=ballot:::dListYlim(dList),
main='Conservative faction \'10-\'15',
ylab='Density',
xlab='SDs from Population mean'
)
col <- seq_along(dList)+1
testFile <- paste('test/pics/fingerprints/Conservative_norm.png')
testPng(testFile)
do.call(plot,arg)
x <- seq(arg$xlim[1],arg$xlim[2],length.out=256)
lines(x,dnorm(x),lty=2)
lapply(seq_along(dList),
	function(i){
		lines(dList[[i]],col=col[i],lwd=2)
	}
)
legend('topright',legend=names(sbListCon),col=col,lwd=2)
dev.off()
if(buildPackageLoaded)gitAdd(print(testFile))




sbListLab <- list(Labour.2010=GE2010$Lab,
	Labour.2015=GE2015$Labour
)

dList <- lapply(sbListLab,sbDensity,norm=FALSE)
arg <- list(x=0,
type='n',
xlim=ballot:::dListXlim(dList),
ylim=ballot:::dListYlim(dList),
main='Labour faction \'10-\'15',
ylab='Density',
xlab='Fractional Turnout'
)
col <- seq_along(dList)+1
testFile <- paste('test/pics/fingerprints/Labour.png')
testPng(testFile)
do.call(plot,arg)
lapply(seq_along(dList),
function(i){
lines(dList[[i]],col=col[i],lwd=2)
		x <- seq(min(dList[[i]]$x),max(dList[[i]]$x),length.out=256)
		lines(x,dnorm(x,mean=sbPopMean(sbList[[i]]),sd=sbPopSd(sbList[[i]])),lty=2)
}
)
legend('topright',legend=names(sbListLab),col=col,lwd=2)
dev.off()
if(buildPackageLoaded)gitAdd(print(testFile))
dList <- lapply(sbListLab,sbDensity,norm=TRUE)
arg <- list(x=0,
type='n',
xlim=ballot:::dListXlim(dList),
ylim=ballot:::dListYlim(dList),
main='Labour faction \'10-\'15',
ylab='Density',
xlab='SDs from Population mean'
)
col <- seq_along(dList)+1
testFile <- paste('test/pics/fingerprints/Labour_norm.png')
testPng(testFile)
do.call(plot,arg)
x <- seq(arg$xlim[1],arg$xlim[2],length.out=256)
lines(x,dnorm(x),lty=2)
lapply(seq_along(dList),
	function(i){
		lines(dList[[i]],col=col[i],lwd=2)
	}
)
legend('topright',legend=names(sbListLab),col=col,lwd=2)
dev.off()
if(buildPackageLoaded)gitAdd(print(testFile))


sbListLD <- list(Liberal.Democrat.2010=GE2010$LD,
	Liberal.Democrat.2015=GE2015$Lib.Dem
)

dList <- lapply(sbListLD,sbDensity,norm=FALSE)
arg <- list(x=0,
type='n',
xlim=ballot:::dListXlim(dList),
ylim=ballot:::dListYlim(dList),
main='Liberal Democrat faction \'10-\'15',
ylab='Density',
xlab='Fractional Turnout'
)
col <- seq_along(dList)+1
testFile <- paste('test/pics/fingerprints/Liberal.Democrat.png')
testPng(testFile)
do.call(plot,arg)
lapply(seq_along(dList),
function(i){
lines(dList[[i]],col=col[i],lwd=2)
		x <- seq(min(dList[[i]]$x),max(dList[[i]]$x),length.out=256)
		lines(x,dnorm(x,mean=sbPopMean(sbList[[i]]),sd=sbPopSd(sbList[[i]])),lty=2)
}
)
legend('topright',legend=names(sbListLD),col=col,lwd=2)
dev.off()
if(buildPackageLoaded)gitAdd(print(testFile))
dList <- lapply(sbListLD,sbDensity,norm=TRUE)
arg <- list(x=0,
type='n',
xlim=ballot:::dListXlim(dList),
ylim=ballot:::dListYlim(dList),
main='Liberal Democrat faction \'10-\'15',
ylab='Density',
xlab='SDs from Population mean'
)
col <- seq_along(dList)+1
testFile <- paste('test/pics/fingerprints/Liberal.Democrat_norm.png')
testPng(testFile)
do.call(plot,arg)
x <- seq(arg$xlim[1],arg$xlim[2],length.out=256)
lines(x,dnorm(x),lty=2)
lapply(seq_along(dList),
	function(i){
		lines(dList[[i]],col=col[i],lwd=2)
	}
)
legend('topright',legend=names(sbListLD),col=col,lwd=2)
dev.off()
if(buildPackageLoaded)gitAdd(print(testFile))

