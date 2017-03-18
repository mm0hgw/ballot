system('mkdir -p test/pics/')

lapply(ls.layerTag(),
	function(layerTagName){
		testFile <- paste(sep='','test/pics/',layerTagName,'_layerTag_test.png')
		testPng(testFile)
		plot(layerTag(layerTagName))
		dev.off()
		if(buildPackageLoaded)gitAdd(print(testFile))
	}
)

lapply(ls.regionTag(),
	function(regionTagName){
		testFile <- paste(sep='','test/pics/',regionTagName,'_regionTag_test.png')
		testPng(testFile)
		plot(regionTag(regionTagName))
		dev.off()
		if(buildPackageLoaded)gitAdd(print(testFile))
	}
)

lapply(ls.ballotTag(),
	function(ballotTagName){
		x <- as.ballotTag(ballotTagName)
		testFile <- paste(sep='','test/pics/',ballotTagName,'_ballotTag_test_0.1.png')
		a<-strsplit(x,'\\.')[[1]]
		year<-a[length(a)-1]
		testPng(testFile)
		plot(x)
		dev.off()
		if(buildPackageLoaded)gitAdd(print(testFile))
		sbList <- splitBallot(get.ballot(x))[seq(3)]
		sampleList <- lapply(sbList,sbCalculateSample,norm=TRUE)
		testFile <- paste(sep='','test/pics/',ballotTagName,'_ballotTag_test_0.2.png')
		testPng(testFile)
		do.call(rainbowPlot,sampleList)
		dev.off()
		if(buildPackageLoaded)gitAdd(print(testFile))		
		lapply(seq_along(sbList),
			function(i){
				name <- sub('^V$','Overall Turnout',names(sbList)[i])
				if(length(grep('^SF$|^Sinn',name))>0)
					name <- 'Sinn Fein'
				sb <- sbList[[i]]
				testFile <- paste(sep='',
					'test/pics/',ballotTagName,'_ballotTag_test_',i,'_',
					gsub(' ','.',name),'.png'
				)
				title <- paste(sep=', ',name,year)
				subTitle <- paste(sep='/',sbCharSum(sb),sbCharSumN(sb))
				testPng(testFile)
				spatialPlot(x,
					sample=sbCalculateSample(sb,norm=TRUE),
					main=title, sub=subTitle
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

system('mkdir -p test/pics/fp/')

testFile <- paste('test/pics/fp//2010_SNP.png')
testPng(testFile)


