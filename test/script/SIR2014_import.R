load("test/data/elections.RData")
SIR2014_job <- list('UK.Scotland.2014.SIR',
	ballot=elections$SIR2014,
	bRegionTag='UK.Scotland.2014.SIR',
	bTitle='Scottish Independence Referendum, 2014'
)
load('test/data/KS2012.ballot.rda')
KS2012_job <- list('KS2012',
	ballot=KS2012.ballot,
	bRegionTag='KS2010',
	bTitle=KS2012.bTitle
)

load('test/data/KS2016.ballot.rda')
KS2016_job <- list('KS2016',
	ballot=KS2016.ballot,
	bRegionTag='KS2010',
	bTitle=KS2016.bTitle
)

do.call(ballotTag,SIR2014_job)
do.call(ballotTag,KS2012_job)
do.call(ballotTag,KS2016_job)
