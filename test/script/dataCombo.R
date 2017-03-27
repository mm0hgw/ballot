z<-ls.ballotTag('SIR')
z
z.combo <- get.combo (z)
z.ballot <- get.ballot(z)
z.sbList <- splitBallot(z.ballot)
z.sp <- get.Spatial (z)
z.sp.dc <- ultraCombo::dataCombo(z.combo,z.sp,invisible,TRUE)
oldMar <- par('mar')
system('mkdir -p test/pics/dc')
system('rm test/pics/dc/*')
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
lapply(z.sbList,
	function(z.sb){
		z.sbChisqTest.dc <- ultraCombo::dataCombo(z.combo,z.sb,sbChisqTest,TRUE)
		ch <- do.call(c,
			LAPPLYFUN(comboChunk(z.sbChisqTest.dc,by=chunkSize),
				function(dc){
					sapply(seq(dc$len),dc$dGen)
				}
			)
		)
		which.max(ch)
	}
)
