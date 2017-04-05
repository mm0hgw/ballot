system('mkdir -p test/pics/layers/')

lapply(ls.layerTag(),
	function(layerTagName){
		cat('layer plot',layerTagName,'\n')
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
		cat('region plot',regionTagName,'\n')
		testFile <- paste(sep='','test/pics/regions/',regionTagName,'_regionTag_test.png')
		testPng(testFile)
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
			ballotTagName,'_ballotTag_test.png'
		)
		a<-strsplit(x,'\\.')[[1]]
		year<-a[length(a)-1]
		testPng(testFile)
		plot(x)
		dev.off()
		if(buildPackageLoaded)gitAdd(print(testFile))
		testFile <- paste(sep='','test/pics/ballot/',
			ballotTagName,'_ballotTag_test.csv'
		)
		write.csv(get.ballot(x),file=testFile)
		if(buildPackageLoaded)gitAdd(print(testFile))
	}
)

system('zip -9ju test/pics/basic.zip test/pics/layers/* test/pics/regions/* test/pics/ballot/*')
if(buildPackageLoaded)gitAdd('test/pics/basic.zip')
