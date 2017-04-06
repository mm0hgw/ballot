baseDir <- 'test/pics/fingerprints2/'

system(paste('mkdir','-p',baseDir))
KS2012 <- splitBallot(get.ballot(ls.ballotTag('KS.2012')))
KS2016 <- splitBallot(get.ballot(ls.ballotTag('KS.2016')))
KS2012[[1]] <- sbAbstainers(KS2012[[1]])
KS2016[[1]] <- sbAbstainers(KS2016[[1]])
names(KS2012)[1] <- 'Abstainers'
names(KS2016)[1] <- 'Abstainers'
bList <- list(KS2012,KS2016)

parties <- intersect(names(KS2012),names(KS2016))

sbTags <- lapply(c('KS.2012','KS.2016'),
		ls.ballotTag
	)
sbNames <- sapply(sbTags,as.character)

do.party <- function(party,baseDir,bList){
	sbList <- lapply(bList,'[',party)
	name <- party
	dList <- lapply(sbList,sbDensity,norm=FALSE)
	arg <- list(x=0,
		type='n',
		xlim=ballot:::dListXlim(dList),
		ylim=ballot:::dListYlim(dList),
		main=paste(name,'\'10-\'15'),
		ylab='Density',
		xlab='Fractional Turnout'
	)
	col <- seq_along(dList)+1
	testFile <- paste(sep='','test/pics/fingerprints2/',name,'_sample.png')
	testPng(testFile)
	do.call(plot,arg)
	lapply(seq_along(dList),
		function(i){
			lines(dList[[i]],col=col[i],lwd=2)
			x <- seq(min(dList[[i]]$x),max(dList[[i]]$x),length.out=256)
			lines(x,dnorm(x,mean=sbPopMean(sbList[[i]]),sd=sbPopSd(sbList[[i]])),lty=2)
		}
	)
	leg <- paste( sbNames,
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
			sb <- sbList[[i]]
			sample <- sbCalculateSample(sb)
			l <- nchar(names(sbList)[i])
			year <- substr(names(sbList)[i],l-3,l)
			testFile <- paste(sep='',baseDir,name,'.',year,'.csv')
			csvTmp <- cbind(sb,sample)
			csvTmp <- csvTmp[order(sample,decreasing=TRUE),]
			print(csvTmp)
			write.csv(file=testFile,csvTmp)
			if(buildPackageLoaded)gitAdd(print(testFile))			
			testFile <- paste(sep='',baseDir,name,'.',year,'.png')
			testPng(testFile)
			tag <- ls.ballotTag(paste(sep='.','Scotland',year))
			spatialPlot(tag,
				sample= sbCalculateSample(sb,norm=FALSE) * 100,
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
	testFile <- paste(sep='',baseDir,name,'_norm.png')
	testPng(testFile)
	do.call(plot,arg)
	x <- seq(arg$xlim[1],arg$xlim[2],length.out=256)
	lines(x,dnorm(x),lty=2)
	lapply(seq_along(dList),
		function(i){
			lines(dList[[i]],col=col[i],lwd=2)
		}
	)
	legend('topright',legend=leg,col=col,lwd=2)
	dev.off()
	if(buildPackageLoaded)gitAdd(print(testFile))
}

lapply(parties,do.party,baseDir,bList)
