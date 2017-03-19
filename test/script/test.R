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
		sbList <- splitBallot(get.ballot(x))
		lapply(seq_along(sbList),
			function(i){
				name <- sub('^V$','Overall Turnout',names(sbList)[i])
				if(length(grep('^SF$|^Sinn',name))>0)
					name <- 'Sinn Fein'
				sb <- sbList[[i]]
				testFile <- paste(sep='',
					'test/pics/ballot/sp/',ballotTagName,'_ballotTag_test_',i,'_',
					gsub(' ','.',name),'.png'
				)
				title <- paste(sep=', ',name,year)
				subTitle <- paste(sep='/',sbSum(sb),sbSumN(sb))
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
