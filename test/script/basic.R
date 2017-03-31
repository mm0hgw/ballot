system('mkdir -p test/pics/layers/')

lapply(ls.layerTag(),
	function(layerTagName){
		testFile <- paste(sep='','test/pics/layers/',layerTagName,'_layerTag_test.png')
		testPng(testFile)
		plot(layerTag(layerTagName))
		dev.off()
		if(buildPackageLoaded)gitAdd(print(testFile))
	}
)

system('mkdir -p test/pics/regions/')

lapply(ls.regionTag(),
	function(regionTagName){
		testFile <- paste(sep='','test/pics/regions/',regionTagName,'_regionTag_test.png')
		testPng(testFile)
		plot(regionTag(regionTagName))
		dev.off()
		if(buildPackageLoaded)gitAdd(print(testFile))
	}
)

system('mkdir -p test/pics/ballot/sp')


lapply(ls.ballotTag(),
	function(ballotTagName){
		x <- as.ballotTag(ballotTagName)
		testFile <- paste(sep='','test/pics/ballot/',
			ballotTagName,'_ballotTag_test_0.1.png'
		)
		a<-strsplit(x,'\\.')[[1]]
		year<-a[length(a)-1]
		testPng(testFile)
		plot(x)
		dev.off()
		if(buildPackageLoaded)gitAdd(print(testFile))
	}
)
