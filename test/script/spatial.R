
system('mkdir -p test/pics/spatial')

lapply(ls.ballotTag(),
	function(ballotTagName){
		x <- as.ballotTag(ballotTagName)
		cat('spatial plot',x,'\n')
		sbList <- splitBallot(get.ballot(x))[seq(3)]
		lapply(seq_along(sbList),
			function(i){
				name <- sub('^V$','Overall Turnout',names(sbList)[i])
				if(length(grep('^SF$|^Sinn',name))>0)
					name <- 'Sinn Fein'
				sb <- sbList[[i]]
				testFile <- paste(sep='',
					'test/pics/spatial/',ballotTagName,'_ballotTag_test_',i,'_',
					gsub(' ','.',name),'.png'
				)
				a<-strsplit(x,'\\.')[[1]]
				year<-a[length(a)-1]
				title <- paste(sep=', ',name,year)
				subTitle <- paste(sep='/',sbSum(sb),sbSumN(sb))
				testPng(testFile)
				spatialPlot(x,
					sample=as.integer(format(digits=3,sbCalculateSample(sb,norm=FALSE)*100)),
					main=title, sub=subTitle
				)
				dev.off()
				if(buildPackageLoaded)gitAdd(print(testFile))
			}
		)
	}
)

system('zip -9ju test/pics/spatial.zip test/pics/spatial/*')
if(buildPackageLoaded)gitAdd('test/pics/spatial.zip')
