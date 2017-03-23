z<-ls.ballotTag('SIR.2014')
z
z.combo <- get.combo (z)
z.ballot <- get.ballot(z)
z.sp <- get.Spatial (z)
z.dc <- dataCombo(z.combo,z.sp,)


system('mkdir -p test/pics/dc')
lapply(seq(1),#z.dc$len),
	function(i){
		fileName <- paste(sep='','test/pics/dc/',z.dc$combo$i[i],'.png')
		png(width=50,height=50,fileName)
		plot(z.dc$Gen(i))
		dev.off()
		gitAdd(fileName)
	}
)
