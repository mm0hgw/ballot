lapply(ls.layerTag(),
	function(layerTagName){
		testFile <- paste(sep='','test/pics/layerTag_test_',layerTagName,'.png')
		testPng(testFile)
		plot(layerTag(layerTagName))
		dev.off()
		if(buildPackageLoaded)gitAdd(print(testFile))
	}
)
lapply(ls.regionTag(),
	function(regionTagName){
		testFile <- paste(sep='','test/pics/regionTag_test_',regionTagName,'.png')
		testPng(testFile)
		plot(regionTag(regionTagName))
		dev.off()
		if(buildPackageLoaded)gitAdd(print(testFile))
	}
)
lapply(ls.ballotTag(),
	function(ballotTagName){
		x <- as.ballotTag(ballotTagName)
		testFile <- paste(sep='','test/pics/ballotTag_test_',ballotTagName,'.png')
		testPng(testFile)
		plot(x)
		dev.off()
		if(buildPackageLoaded)gitAdd(print(testFile))
		sbList <- splitBallot(get.ballot(x))[seq(3)]
		lapply(seq_along(sbList),
			function(i){
				name <- sub('^V$','Overall Turnout',names(sbList)[i])
				sb <- sbList[[i]]
				testFile <- paste(sep='',
					'test/pics/spatialPlot_',ballotTagName,'_',i,'_',
					gsub(' ','.',name),'.png'
				)
				testPng(testFile)
				spatialPlot(x,
					sample=sbCalculateSample(sb,norm=TRUE),
					main=paste(sep=', ',name,get.bTitle(x))
				)
				dev.off()
				if(buildPackageLoaded)gitAdd(print(testFile))
#				testFile <- paste(sep='',
#					'test/pics/freqPlot_',ballotTagName,'_',i,'_',
#					gsub(' ','.',name),'.png'
#				)
#				testPng(testFile)
#				freqPlot(x,i)
#				dev.off()
#				if(buildPackageLoaded)gitAdd(print(testFile))
			}
		)
	}
)

