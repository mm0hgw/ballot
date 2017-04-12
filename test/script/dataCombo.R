z<-ls.ballotTag('SIR')
z
fingerprint <- function(z){
	z <- as.ballotTag(z)
	stopifnot(is.ballotTag(z))
	z.combo <- get.combo(z)
	z.ballot <- get.ballot(z)
	elemNames <- rownames(z.ballot)
	z.sbList <- splitBallot(z.ballot)
	z.sbList[[1]] <- sbAbstainers(z.sbList[[1]])
	names(z.sbList)[1] <- 'Abstainers'
	z.sbList <- z.sbList[sapply(z.sbList,sbSum)!=0]
	z.sbList <- head(z.sbList,n=3)
	z.sp <- get.Spatial (z)
	z.sp.dc <- ultraCombo::dataCombo(z.combo,z.sp)
	oldMar <- par('mar')
	system('mkdir -p test/pics/dc')
	z.sbNames <- names(z.sbList)
	require(mclapplyFunGen)
	LAPPLYFUN <- get.lapply::get.lapply()
	chunkSize <- get.lapply::get.chunkSize()
	par(mar=oldMar)
	lapply(seq_along(z.sbList),
		function(i){
			zExplode <- strsplit(z,'.')[[1]]
			zDir <- paste(sep='.',zExplode[-seq(to=length(zExplode),length.out=2)])
			party <- z.sbNames[i]
			z.chisq <- get.chisq(z,party)
			jList <- head(n=7,order(z.chisq,decreasing=TRUE))
			maxch <- z.chisq[jList]
			rm(z.chisq)
			z.sb <- z.sbList[[i]]
			z.dc <- ultraCombo::dataCombo(z.combo,z.sb)
			for(j in jList){
				testFile <- paste(sep='',
					'test/pics/',zDir,'dc/',z,'_',
					gsub(' ','.',z.sbNames[i]),'_density_',z.combo$i[j],'.png'
				)
				testPng(testFile)
				sb <- z.dc$dGen(j)
				sample <- sbCalculateSample(sb)
				csvFile <- paste(sep='',
					'test/pics/',zDir,'dc/',z,'_',
					gsub(' ','.',z.sbNames[i]),'_results_',z.combo$i[j],'.csv'
				)
				write.csv(
					cbind(z.dc$dGen(j),sample)[order(sample,decreasing=TRUE),],
					file=csvFile
				)
				if(buildPackageLoaded)gitAdd(print(csvFile))
				col<-sample_to_color(sample)
				ord <- order(sample)
				subTitle <- paste(collapse=', ',elemNames[z.combo$Gen(j)[ord]])
				dObj <- density (sample)
				plot(dObj,
					main=paste(get.bTitle(z),z.combo$i[j],z.sbNames[i],
						'Chisq:',format(maxch,digits=5)
					),
					sub=subTitle
				)
				lines(dObj$x,
					dnorm(dObj$x,
						mean=sbPopMean(sb),
						sd=sbPopSd(sb)
					),
					lty=2				
				)
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
					col=col,
					pch=1,
					lwd=5
				)
				dev.off()
				if(buildPackageLoaded)gitAdd(print(testFile))
				testFile <- paste(sep='',
					'test/pics/',zDir,'/dc/',z,'_',
					gsub(' ','.',z.sbNames[i]),'_spatial_',z.combo$i[j],'.png'
				)
				testPng(testFile)
				par(mar=rep(0,4))
				sp<-z.sp.dc$dGen(j)
				sp::plot(z.sp,border='grey')
				sp::plot(sp,border='black',col=col,add=TRUE)
				dev.off()
				if(buildPackageLoaded)gitAdd(print(testFile))
			}
		}
	)
}

lapply(ls.ballotTag(),fingerprint)
