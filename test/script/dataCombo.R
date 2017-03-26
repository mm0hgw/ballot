z<-ls.ballotTag('SIR')
z
z.combo <- get.combo (z)
z.ballot <- get.ballot(z)
z.sp <- get.Spatial (z)
z.dc <- ultraCombo::dataCombo(z.combo,z.sp,invisible,TRUE)
oldMar <- par('mar')
system('mkdir -p test/pics/dc')
system('rm test/pics/dc/*')
lapply(seq(100),#z.dc$len),
	function(i){
		fileName <- paste(sep='','test/pics/dc/',z.dc$i[i],'.png')
		png(width=40,height=40,fileName)
		par(mar=rep(0,4))
		sp::plot(z.dc$dGen(i))
		dev.off()
	}
)
system('zip -9vju test/pics/dc.zip test/pics/dc/*')
gitAdd('test/pics/dc.zip')
par(mar=oldMar)
