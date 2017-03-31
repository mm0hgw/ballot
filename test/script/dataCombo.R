z<-ls.ballotTag('SIR')
z
fingerprint <- function(z){
	z <- as.ballotTag(z)
	stopifnot(is.ballotTag(z))
	z.combo <- get.combo(z)
	z.ballot <- get.ballot(z)
	z.sbList <- splitBallot(z.ballot)
	z.sbList[[1]] <- sbAbstainers(z.sbList[[1]])
	names(z.sbList)[1] <- 'Abstainers'
	z.sbList <- sortBallot(z.ballot)
	z.sbList <- z.sbList[sapply(z.sbList,sbSum)!=0]
	z.sbList <- head(z.sbList,n=3)
	print(z.sbList)
	z.sp <- get.Spatial (z)
	z.sp.dc <- ultraCombo::dataCombo(z.combo,z.sp,invisible,TRUE)
	oldMar <- par('mar')
	system('mkdir -p test/pics/dc/fp')
	require(mclapplyFunGen)
	LAPPLYFUN <- get.lapply::get.lapply()
	chunkSize <- get.lapply::get.chunkSize()
	system('zip -9vju test/pics/dc.zip test/pics/dc/*')
	gitAdd('test/pics/dc.zip')
	par(mar=oldMar)
	lapply(seq_along(z.sbList),
		function(i){
			z.sb <- z.sbList[[i]]
			z.sbChisqTest.dc <- ultraCombo::dataCombo(z.combo,z.sb,sbChisqTest,TRUE)
			ch <- do.call(c,
				LAPPLYFUN(ultraCombo::comboChunk(z.sbChisqTest.dc,by=chunkSize),
					function(dc){
						sapply(seq(dc$len),dc$dGen)
					}
				)
			)
			j <- which.max(ch)
			maxch <- max(ch)
			rm(ch)
			z.sbDensity.dc <- ultraCombo::dataCombo(z.combo,z.sb,sbDensityGen(norm=TRUE),TRUE)
			fileName <- paste(sep='',
				'test/pics/dc/fp/',z,'_',z.combo$i[j],'_',names(z.sbList)[i],'.png'
			)
			testPng(fileName)
			plot(z.sbDensity.dc$dGen(j),
				main=paste(get.bTitle(z),z.combo$i[j],names(z.sbList)[i],'Chisq:',format(maxch,digits=5)))
			dev.off()
			gitAdd(print(fileName))
			fileName <- paste(sep='','test/pics/dc/fp/',z,'_',z.sp.dc$i[j],'.png')
			png(width=100,height=100,fileName)
			par(mar=rep(0,4))
			sp::plot(z.sp.dc$dGen(j))
			dev.off()
			gitAdd(print(fileName))
		}
	)
}

lapply(ls.ballotTag('SIR'),fingerprint)
lapply(ls.ballotTag('KS'),fingerprint)
