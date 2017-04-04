system('mkdir -p test/pics/fingerprints/sp/')
GE2010 <- splitBallot(get.ballot(ls.ballotTag('Scotland.2010')))
SIR2014 <- splitBallot(get.ballot(ls.ballotTag('Scotland.2014')))
GE2015 <- splitBallot(get.ballot(ls.ballotTag('Scotland.2015')))

do.sbList <- function(sbList,name){
	dList <- lapply(sbList,sbDensity,norm=FALSE)
	arg <- list(x=0,
		type='n',
		xlim=ballot:::dListXlim(dList),
		ylim=ballot:::dListYlim(dList),
		main=paste(name,'faction \'10-\'15'),
		ylab='Density',
		xlab='Fractional Turnout'
	)
	col <- seq_along(dList)+1
	testFile <- paste(sep='','test/pics/fingerprints/',name,'.png')
	testPng(testFile)
	do.call(plot,arg)
	lapply(seq_along(dList),
		function(i){
			lines(dList[[i]],col=col[i],lwd=2)
			x <- seq(min(dList[[i]]$x),max(dList[[i]]$x),length.out=256)
			lines(x,dnorm(x,mean=sbPopMean(sbList[[i]]),sd=sbPopSd(sbList[[i]])),lty=2)
		}
	)
	leg <- paste( names(sbList),
		format(digits=4,sapply(sbList,sbPopMean)*100),
		'%',
		sapply(sbList,sbSum),
		'/',
		sapply(sbList,sbSumN)
	)
	legend('topright',legend=leg,col=col,lwd=2)
	dev.off()
	if(buildPackageLoaded)gitAdd(print(testFile))
	lapply(seq_along(sbList),
		function(i){
			l <- nchar(names(sbList)[i])
			year <- substr(names(sbList)[i],l-3,l)
			testFile <- paste(sep='','test/pics/fingerprints/',name,'.',year,'.png')
			testPng(testFile)
			tag <- ls.ballotTag(paste(sep='.','Scotland',year))
			spatialPlot(tag,
				sample= sbCalculateSample(sbList[[i]],norm=FALSE) * 100,
				main=paste(get.bTitle(tag),names(sbList)[i])
			)
			dev.off()
			if(buildPackageLoaded)gitAdd(print(testFile))			
		}
	)
	
	dList <- lapply(sbList,sbDensity,norm=TRUE)
	arg <- list(x=0,
		type='n',
		xlim=ballot:::dListXlim(dList),
		ylim=ballot:::dListYlim(dList),
		main=paste(name,'faction \'10-\'15'),
		ylab='Density',
		xlab='SDs from Population mean'
	)
	col <- seq_along(dList)+1
	testFile <- paste(sep='','test/pics/fingerprints/',name,'_norm.png')
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
}
sbListYes <- list(YeSNP2010=GE2010$SNP,
	YeSNP2014=SIR2014$Yes,
	YeSNP2015=GE2015$SNP
)

sbListAbstain <- list(Abstainers.2010=sbAbstainers(GE2010$V),
Abstainers.2014=sbAbstainers(SIR2014$V),
Abstainers.2015=sbAbstainers(GE2015$V)
)

GE2010U <- GE2010$V
GE2010U[,2] <- GE2010$V[,2] - GE2010$SNP[,2]
GE2015U <- GE2015$V
GE2015U[,2] <- GE2015$V[,2] - GE2015$SNP[,2]

sbListNo <- list( Unionist.2010=GE2010U,
Unionist.2014=SIR2014$No,
Unionist.2015=GE2015U
)
sbListCon <- list(Conservative.2010=GE2010$Con,
	Conservative.2015=GE2015$Conservative
)

sbListLab <- list(Labour.2010=GE2010$Lab,
	Labour.2015=GE2015$
	Labour
)

sbListLD <- list(Liberal.Democrat.2010=GE2010$LD,
	Liberal.Democrat.2015=GE2015$Lib.Dem
)


do.sbList(sbListYes,'YeSNP')
do.sbList(sbListNo,'Unionist')
do.sbList(sbListAbstain,'Abstainers')
do.sbList(sbListCon,'Conservative')
do.sbList(sbListLab,'Labour')
do.sbList(sbListLD,'Liberal.Democrats')



