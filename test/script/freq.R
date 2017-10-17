
lapply( ls.ballotTag(),
	function(tag){
		fileName <- paste(sep='', 'test/pics/freq/',tag,'.png')
		png(fileName)
		freqPlot(tag)
		dev.off()
		if(buildPackageLoaded)gitAdd(print(fileName))
	}
) 

