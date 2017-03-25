z<-ls.ballotTag('SIR')
z
z.combo <- get.combo (z)
z.ballot <- get.ballot(z)
z.sp <- get.Spatial (z)
z.dc <- ultraCombo::dataCombo(z.combo,z.sp,sbDensity,TRUE)
oldMar <- par('mar')
system('mkdir -p test/pics/dc')
lapply(seq(1),#z.dc$len),
	function(i){
		fileName <- paste(sep='','test/pics/dc/',z.dc$combo$i[i],'.png')
		png(width=100,height=100,fileName)
		par(mar=rep(0,4))
		sp::plot(z.dc$Gen(i))
		dev.off()
	}
)
system('tar -jvvc --options bzip2:compression-level=9 -f test/pics/dc.tar.bz2 test/pics/dc/*')
gitAdd('test/pics/dc.bzip2')
par(mar=oldMar)
