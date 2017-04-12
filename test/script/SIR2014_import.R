load("test/data/elections.RData")
SIR2014_job <- list('UK.Scotland.2014.SIR',
	ballot=elections$SIR2014,
	bRegionTag='UK.Scotland.2014.SIR',
	bTitle='Scottish Independence Referendum, 2014'
)
load('test/data/KS2012.ballot.rda')
rownames(KS2012.ballot) <- gsub('Chautaqua','Chautauqua',rownames(KS2012.ballot))
KS2012_job <- list('US.KS.2012.Pres',
	ballot=KS2012.ballot,
	bRegionTag='KS2010',
	bTitle=KS2012.bTitle
)
rMask <- get.rMask('KS2010.4th')
KS2012_job2 <- list('US.KS.4th.2012.Pres',
	ballot=KS2012.ballot[rMask,],
	bRegionTag='KS2010.4th',
	bTitle=KS2012.bTitle
)

load('test/data/KS2016.ballot.rda')
rownames(KS2016.ballot) <- gsub('Chautaqua','Chautauqua',rownames(KS2016.ballot))
KS2016_job <- list('US.KS.2016.Pres',
	ballot=KS2016.ballot,
	bRegionTag='KS2010',
	bTitle=KS2016.bTitle
)
KS2016_job2 <- list('US.KS.4th.2016.Pres',
	ballot=KS2016.ballot[rMask,],
	bRegionTag='KS2010.4th',
	bTitle=KS2016.bTitle
)

KS2017.ballot <- read.csv('test/data/KS2017.csv')
rownames(KS2017.ballot) <- KS2017.ballot[,1]
KS2017.ballot[,1] <- KS2016.ballot[fourthRep,'N']
colnames(KS2017.ballot)[1] <- 'N'
KS2017.ballot <- sortBallot(cbind(KS2017.ballot,V=rowSums(KS2017.ballot[,-1])))
KS2017_job <- list('US.KS.4th.2017.Rep',
	ballot=KS2017.ballot,
	bRegionTag='KS2010.4th',
	bTitle=KS2016.bTitle
)

do.call(ballotTag,SIR2014_job)
do.call(ballotTag,KS2012_job)
do.call(ballotTag,KS2016_job)
do.call(ballotTag,KS2012_job2)
do.call(ballotTag,KS2016_job2)
do.call(ballotTag,KS2017_job)
