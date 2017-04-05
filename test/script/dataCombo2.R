fingerprint <- function(z){
	z <- as.ballotTag(z)
	stopifnot(is.ballotTag(z))
	z.combo <- get.combo(z)
	z.cnt <- plyr::count(as.vector(z.combo$Gen(seq(z.combo$len))))
	print(z.cnt)
	z.ballot <- get.ballot(z)
	elemNames <- rownames(z.ballot)
	z.sbList <- splitBallot(z.ballot)
	z.sbList[[1]] <- sbAbstainers(z.sbList[[1]])
	names(z.sbList)[1] <- 'Abstainers'
	z.sbList <- z.sbList[sapply(z.sbList,sbSum)!=0]
	z.sbList <- head(z.sbList,n=3)
	print(z.sbList)
	z.sp <- get.Spatial (z)
	z.sp.dc <- ultraCombo::dataCombo(z.combo,z.sp)
	print(z.sp.dc)
	system('mkdir -p test/pics/dc2')
	z.sbNames <- names(z.sbList)
	lapply(seq_along(z.sbList),
		function(i){
			party <- z.sbNames[i]
			z.chisq <- get.chisq(z,party)
			j <- head(order(z.chisq,decreasing=TRUE),n=z.combo$len%/%10)
			cnt <- plyr::count(as.vector(z.combo$Gen(j)))
			sample <- cnt$freq / z.cnt$freq
			print(sample)
			col <- sample_to_color(sample)
			testFile <- paste(sep='',
				'test/pics/dc2/',z,'_',
				gsub(' ','.',z.sbNames[i]),'_spatial.png'
			)
			testPng(testFile)
			par(mar=rep(0,4))
			sp::plot(z.sp,border='black',col=col)
			dev.off()
			if(buildPackageLoaded)gitAdd(print(testFile))
		}
	)
}

lapply(ls.ballotTag(),fingerprint)
