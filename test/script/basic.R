system('mkdir -p test/pics/layers/')

lapply(ls.layerTag(),
	function(layerTagName){
		cat('layer plot',layerTagName,'\n')
		testFile <- paste(sep='','test/pics/layers/',layerTagName,'_layerTag_test.png')
		testFun(testFile)
		plot(layerTag(layerTagName))
		dev.off()
		if(buildPackageLoaded)gitAdd(print(testFile))
	}
)

system('mkdir -p test/pics/regions/')

lapply(ls.regionTag(),
	function(regionTagName){
		cat('region plot',regionTagName,'\n')
		testFile <- paste(sep='','test/pics/regions/',regionTagName,'_regionTag_test.png')
		testFun(testFile)
		plot(regionTag(regionTagName))
		dev.off()
		if(buildPackageLoaded)gitAdd(print(testFile))
	}
)

system('mkdir -p test/pics/ballot/')

lapply(ls.ballotTag(),
	function(ballotTagName){
		x <- as.ballotTag(ballotTagName)
		cat('ballot plot',x,'\n')
		testFile <- paste(sep='','test/pics/ballot/',
			ballotTagName,'_ballotTag_sample.png'
		)
		a<-strsplit(x,'\\.')[[1]]
		year<-a[length(a)-1]
		testFun(testFile)
		plot(x,norm=FALSE)
		dev.off()
		if(buildPackageLoaded)gitAdd(print(testFile))
		testFile <- paste(sep='','test/pics/ballot/',
			ballotTagName,'_ballotTag_norm.png'
		)
		a<-strsplit(x,'\\.')[[1]]
		year<-a[length(a)-1]
		testFun(testFile)
		plot(x,norm=TRUE)
		dev.off()
		if(buildPackageLoaded)gitAdd(print(testFile))

		testFile <- paste(sep='','test/pics/ballot/',
			ballotTagName,'_ballotTag.csv'
		)
		write.csv(get.ballot(x),file=testFile)
		if(buildPackageLoaded)gitAdd(print(testFile))
	}
)
