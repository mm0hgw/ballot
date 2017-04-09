baseDir <- 'test/pics/fingerprints3/'

system(paste('mkdir','-p',baseDir))
GE2010 <- splitBallot(get.ballot(ls.ballotTag('GB.2010')))
GE2015 <- splitBallot(get.ballot(ls.ballotTag('GB.2015')))
GE2010[[1]] <- sbAbstainers(GE2010[[1]])
GE2015[[1]] <- sbAbstainers(GE2015[[1]])
names(GE2010)[1] <- 'Abstainers'
names(GE2015)[1] <- 'Abstainers'
bList <- list(GE2010,GE2015)

parties <- intersect(names(GE2010),names(GE2015))

print(parties)

sbTags <- ls.ballotTag('GB')
sbNames <- sapply(sbTags,as.character)
names(bList)<-sbTags
names(bList) <- sbNames

do.party <- function(party,baseDir,bList){
	sbList <- lapply(bList,'[[',party)
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
	testFile <- paste(sep='',baseDir,gsub(' ','.',name),'_sample.png')
	testPng(testFile)
	do.call(plot,arg)
	lapply(seq_along(dList),
		function(i){
			dObj <- dList[[i]]
			sb <- sbList[[i]]
			if(sbSum(sb)!=0){
				sample <- sbCalculateSample(sb)
				pcol <- sample_to_color(sample)
				lines(dObj,col=col[i])
				x <- seq(min(dObj$x),max(dObj$x),length.out=256)
				lines(x,dnorm(x,mean=sbPopMean(sb),sd=sbPopSd(sb)),lty=2)
				points(
					do.call(rbind,
						lapply(seq_along(sample),
							function(i){
								i <- which.min(abs(dObj$x-sample[i]))
								c(x=dObj$x[i],
									y=dObj$y[i]
								)
							}
						)
					),
					col=pcol,
					pch=i,
					lwd=2
				)
			}
		}
	)
	leg <- paste( names(bList),
		format(digits=4,sapply(sbList,sbPopMean)*100),
		'%',
		sapply(sbList,sbSum),
		'/',
		sapply(sbList,sbSumN)
	)
	legend('topright',legend=leg,col=col,lwd=2,pch=seq_along(leg))
	dev.off()
	if(buildPackageLoaded)gitAdd(print(testFile))
	lapply(seq_along(sbList),
		function(i){
			sb <- sbList[[i]]
			sample <- sbCalculateSample(sb)
			y1 <- strsplit(names(bList)[i],'.')
			year <- y1[length(y1)]
			testFile <- paste(sep='',baseDir,gsub(' ','.',name),'.',year,'.csv')
			csvTmp <- cbind(sb,sample)
			csvTmp <- csvTmp[order(sample,decreasing=TRUE),]
			write.csv(file=testFile,csvTmp)
			if(buildPackageLoaded)gitAdd(print(testFile))			
			testFile <- paste(sep='',baseDir,gsub(' ','.',name),'.',year,'.png')
			testPng(testFile)
			tag <- names(bList)[i]
			spatialPlot(tag,
				sample= sbCalculateSample(sb,norm=FALSE) * 100,
				main=paste(get.bTitle(tag),name)
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
		main=paste(name,'\'10-\'15'),
		ylab='Density',
		xlab='SDs from Population mean'
	)
	col <- seq_along(dList)+1
	testFile <- paste(sep='',baseDir,gsub(' ','.',name),'_norm.png')
	testPng(testFile)
	do.call(plot,arg)
	x <- seq(arg$xlim[1],arg$xlim[2],length.out=256)
	lines(x,dnorm(x),lty=2)
	lapply(seq_along(dList),
		function(i){
			lines(dList[[i]],col=col[i])
			dObj <- dList[[i]]
			sb <- sbList[[i]]
			sample <- sbCalculateSample(sb,norm=TRUE)
			pcol <- sample_to_color(sample)
			points(
				do.call(rbind,
					lapply(seq_along(sample),
						function(i){
							i <- which.min(abs(dObj$x-sample[i]))
							c(x=dObj$x[i],
								y=dObj$y[i]
							)
						}
					)
				),
				col=pcol,
				pch=i,
				lwd=2
			)
		}
	)
	legend('topright',legend=leg,col=col,lwd=2)
	dev.off()
	if(buildPackageLoaded)gitAdd(print(testFile))
}

lapply(parties,do.party,baseDir,bList)
