system('mkdir -p test/pics/layers/')

lapply(ls.layerTag(),
	function(layerTagName){
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
		testFile <- paste(sep='','test/pics/regions/',regionTagName,'_regionTag_test.png')
		testFun(testFile)
		plot(regionTag(regionTagName))
		dev.off()
		if(buildPackageLoaded)gitAdd(print(testFile))
	}
)

system('mkdir -p test/pics/ballot/sp')


lapply(ls.ballotTag(),
	function(ballotTagName){
		x <- as.ballotTag(ballotTagName)
		sbList <- splitBallot(get.ballot(x))[seq(3)]
		lapply(seq_along(sbList),
			function(i){
				name <- sub('^V$','Overall Turnout',names(sbList)[i])
				if(length(grep('^SF$|^Sinn',name))>0)
					name <- 'Sinn Fein'
				sb <- sbList[[i]]
				testFile <- paste(sep='',
					'test/pics/ballot/sp/',ballotTagName,'_ballotTag_test_',i,'_',
					gsub(' ','.',name),testSuffix
				)
				title <- paste(sep=', ',name,year)
				subTitle <- paste(sep='/',sbSum(sb),sbSumN(sb))
				testFun(testFile)
				spatialPlot(x,
					sample=as.integer(format(digits=3,sbCalculateSample(sb,norm=FALSE)*100)),
					main=title, sub=subTitle
				)
				dev.off()
				if(buildPackageLoaded)gitAdd(print(testFile))
#				testFile <- paste(sep='',
#					'test/pics/freqPlot_',ballotTagName,'_',i,'_',
#					gsub(' ','.',name),testSuffix
#				)
#				testFun(testFile)
#				freqPlot(x,i)
#				dev.off()
#				if(buildPackageLoaded)gitAdd(print(testFile))
			}
		)
	}
)
