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
			party <- z.sbNames[i]
			z.chisq <- get.chisq(z,party)
			j <- which.max(z.chisq)
			maxch <- z.chisq[j]
			rm(z.chisq)
			z.sb <- z.sbList[[i]]
			z.dc <- ultraCombo::dataCombo(z.combo,z.sb)
			testFile <- paste(sep='',
				'test/pics/dc/',z,'_',z.combo$i[j],'_',
				gsub(' ','.',z.sbNames[i]),'_density.png'
			)
			testPng(testFile)
			sb <- z.dc$dGen(j)
			sample <- sbCalculateSample(sb)
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
				'test/pics/dc/',z,'_',z.combo$i[j],'_',
				gsub(' ','.',z.sbNames[i]),'_spatial.png'
			)
			testPng(testFile)
			par(mar=rep(0,4))
			sp<-z.sp.dc$dGen(j)
			sp::plot(z.sp,border='grey')
			sp::plot(sp,border='black',col=col,add=TRUE)
			dev.off()
			if(buildPackageLoaded)gitAdd(print(testFile))
		}
	)
}

lapply(setdiff(ls.ballotTag(),ls.ballotTag('GB')),fingerprint)
system('zip -9ju test/pics/dc.zip test/pics/dc/*')
