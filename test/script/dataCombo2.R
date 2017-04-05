z<-ls.ballotTag('SIR')
z
fingerprint <- function(z){
	z <- as.ballotTag(z)
	stopifnot(is.ballotTag(z))
	z.combo <- get.combo(z)
	z.cnt <- plyr::count(as.vector(z.combo$Gen(seq(z.combo$len))))
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
	system('mkdir -p test/pics/dc2')
	z.sbNames <- names(z.sbList)
	require(mclapplyFunGen)
	LAPPLYFUN <- get.lapply::get.lapply()
	chunkSize <- get.lapply::get.chunkSize()
	par(mar=oldMar)
	lapply(seq_along(z.sbList),
		function(i){
			party <- z.sbNames[i]
			z.chisq <- get.chisq(z,party)
			j <- head(order(z.chisq,decreasing=TRUE),n=1000)
			cnt <- plyr::count(as.vector(z.combo$Gen(j)))
			sample <- cnt / z.cnt
			col <- sample_to_color(sample)
			testFile <- paste(sep='',
				'test/pics/dc2/',z,'_',z.combo$i[j],'_',
				gsub(' ','.',z.sbNames[i]),'_spatial.png'
			)
			testPng(testFile)
			par(mar=rep(0,4))
			sp<-z.sp.dc$dGen(j)
			sp::plot(z.sp,border='black',col=col)
			dev.off()
			if(buildPackageLoaded)gitAdd(print(testFile))
		}
	)
}

lapply(ls.ballotTag(),fingerprint)
