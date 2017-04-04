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
	system('mkdir -p test/pics/dc/fp')
	z.sbNames <- names(z.sbList)
	require(mclapplyFunGen)
	LAPPLYFUN <- get.lapply::get.lapply()
	chunkSize <- get.lapply::get.chunkSize()
	par(mar=oldMar)
	lapply(seq_along(z.sbList),
		function(i){
			party <- z.sbNames[i]
			print(party)
			ch <- which.and.max(get.chisq(z,party))
			j <- ch[1]
			maxch <- ch[2]
			z.sb <- z.sbList[[i]]
			z.dc <- ultraCombo::dataCombo(z.combo,z.sb)
			fileName <- paste(sep='',
				'test/pics/dc/',z,'_',z.combo$i[j],'_',gsub(' ','.',z.sbNames[i]),'.png'
			)
			testPng(fileName)
			sb <- z.dc$dGen(j)
			sample <- sbCalculateSample(sb)
			ord <- order(sample)
			subTitle <- paste(collapse=', ',elemNames[z.combo$Gen(j)[ord]])
			dObj <- density (sample)
			plot(dObj,
				main=paste(get.bTitle(z),z.combo$i[j],names(z.sbList)[i],'Chisq:',format(maxch,digits=5)),
				sub=subTitle,
				col=2,
				lwd=2
			)
			lines(dObj$x,
				dnorm(dObj$x,
					mean=sbPopMean(sb),
					sd=sbPopSd(sb)
				),
				lty=2				
			)
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

lapply(ls.ballotTag(),fingerprint)
system('zip -9vju test/pics/dc.zip test/pics/dc/*')
gitAdd('test/pics/dc.zip')
