do.party <- function(party,baseDir,bList){
	sbList <- lapply(bList,'[[',party)
	if(any(sapply(sbList,sbSum)==0))
		return()
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
			sb <- (sbList[[i]])
			if(sbSum(sb)>0){
				sample <- sbCalculateSample(sb)
				y1 <- (names(bList)[i])
				y2 <- (strsplit(y1,'\\.')[[1]])
				year <- (y2[length(y2)-1])
				testFile <- paste(sep='',baseDir,gsub(' ','.',name),'.',year,'.csv')
				csvTmp <- cbind(sb,sample)
				csvTmp <- csvTmp[order(sample,decreasing=TRUE),]
				write.csv(file=testFile,csvTmp)
				if(buildPackageLoaded)gitAdd(print(testFile))			
				testFile <- paste(sep='',baseDir,gsub(' ','.',name),'.',year,'.png')
				testPng(testFile)
				tag <- names(bList)[i]
				spatialPlot(tag,
					sample= sbCalculateSample(sb,norm=FALSE),
					center= sbPopMean(sb),
					main=paste(get.bTitle(tag),name)
				)
				dev.off()
				if(buildPackageLoaded)gitAdd(print(testFile))
			}
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
			if(sbSum(sb)!=0){
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
		}
	)
	legend('topright',legend=leg,col=col,lwd=2)
	dev.off()
	if(buildPackageLoaded)gitAdd(print(testFile))
}

do.region <- function(region,country='UK',ballot='Wm'){
	a <- paste(sep='.',country,gsub(' ','.',region))
	baseDir <- paste(sep='','test/pics/',a,'/')
	system(paste('mkdir','-p',baseDir))
	print(a)
	print(ballot)
	print(ls.ballotTag(a))
	sbTags <- grep(ballot,ls.ballotTag(a),value=TRUE)
	print(sbTags)
	bList <-lapply(sbTags,
		function(x){
			out <- splitBallot(get.ballot(x))
			out[[1]] <- sbAbstainers(out[[1]])
			names(out)[1] <- 'Abstainers'
			out
		}
	)
	parties <- names(bList[[1]])
	i <- 2
	while(i<=length(bList)){
		parties <- intersect(parties,names(bList[[i]]))
		i <- i+1
	}
	sbNames <- sapply(sbTags,as.character)
	names(bList) <- sbNames
	lapply(parties,do.party,baseDir,bList)
}

regions <- unique(lapply(strsplit(ls.ballotTag('Wm'),'\\.'),
		function(x){
			paste(collapse=' ',x[-c(1,length(x)-1,length(x))])
		}
	)
)

lapply(regions,do.region,'Wm')
do.region('KS.4th','US','Pres')
do.region('KS','US','Pres')
