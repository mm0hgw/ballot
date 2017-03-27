z<-ls.ballotTag('SIR')
z
z.combo <- get.combo (z)
z.ballot <- get.ballot(z)
z.sbList <- splitBallot(z.ballot)
z.sp <- get.Spatial (z)
z.sp.dc <- ultraCombo::dataCombo(z.combo,z.sp,invisible,TRUE)
oldMar <- par('mar')
system('mkdir -p test/pics/dc/fp')
lapply(seq(100),#z.dc$len),
	function(i){
		fileName <- paste(sep='','test/pics/dc/',z.sp.dc$i[i],'.png')
		png(width=100,height=100,fileName)
		par(mar=rep(0,4))
		sp::plot(z.sp.dc$dGen(i))
		dev.off()
	}
)
require(mclapplyFunGen)
LAPPLYFUN <- get.lapply::get.lapply()
chunkSize <- get.lapply::get.chunkSize()
system('zip -9vju test/pics/dc.zip test/pics/dc/*')
gitAdd('test/pics/dc.zip')
par(mar=oldMar)
lapply(seq_along(z.sbList),
	function(i){
		z.sb <- sbList[[i]]
		z.sbChisqTest.dc <- ultraCombo::dataCombo(z.combo,z.sb,sbChisqTest,TRUE)
		ch <- do.call(c,
			LAPPLYFUN(ultraCombo::comboChunk(z.sbChisqTest.dc,by=chunkSize),
				function(dc){
					sapply(seq(dc$len),dc$dGen)
				}
			)
		)
		i <- which.max(ch)
		maxch <- max(ch)
		z.sbDensity.dc <- ultraCombo::dataCombo(z.combo,z.sb,sbDensity,TRUE)
		fileName <- paste(sep='',
			'test/pics/dc/fp/',z.combo$i[i],'_',names(sbList)[i],'.png'
		)
		testPng(fileName)
		plot(z.sbDensity.dc$dGen(i),main=paste(z.combo$i[i],names(sbList)[i]))
		dev.off()
		gitAdd(fileName)
	}
)
