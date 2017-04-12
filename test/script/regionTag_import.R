
GB.Wm.5th.sp <- get.Spatial('GB.Wm.5th')
NI.Wm.5th.sp <- get.Spatial('NI.Wm.5th')

GB.Wm.5th.sp$NAME <- sapply(strsplit(as.character(GB.Wm.5th.sp$NAME),' '),
	function(x){
		paste(collapse=' ', gsub(',','',x[1:(length(x)-2)]))
	}
)
load("test/data/elections.RData")
GE2010 <- elections$GE2010
GE2010 <- GE2010[order(rownames(GE2010)),]

NIkey <- GE2010[,1]=="Northern Ireland"
GBkey <- as.factor(GE2010[!NIkey,1])
GBNames <- rownames(GE2010)[!NIkey]
NINames <- rownames(GE2010)[NIkey]

keycount <- plyr::count(GBkey)
regionNames <- as.character(keycount$x)[order(keycount$freq)]

filenames <- gsub(" ",".",regionNames)
filenames <- gsub("&","and",filenames)

jobNames <- filenames
jobNames <- c(filenames, "GB")
jobRegion <- as.list(regionNames)
regionMasks <- lapply( jobRegion, 
	function(r)fuzzyMatch(
		GBNames[GBkey %in% r], 
		GB.Wm.5th.sp$NAME
	)
)
regionMasks[[which.max(jobNames=="GB")]] <- fuzzyMatch(GBNames,GB.Wm.5th.sp$NAME)
regionTags <- paste(sep="","UK.",jobNames,".Wm.5th")
rTitles <- sapply(regionTags,
	function(name){
		a <- strsplit(name,".",fixed=TRUE)[[1]][-seq(2)]
		year <- a[length(a)]
		prettyName <- paste(collapse=" ",a[-length(a)])
		if(prettyName=="GB")
			prettyName <- "Great Britain"
		paste(sep=", ","United Kingdom",prettyName,
			"Westminster","5th Review"
		)
	}
)
lapply(seq_along(jobNames),
	function(i){
		regionTag(regionTags[i],
			rTitles[i],
			layerTag('GB.Wm.5th'),
			regionMasks[[i]]
		)
	}
)

rLayerTag <- 'TIGER2010.KS'
sp.layer <- get.Spatial(rLayerTag)
mask <- order(as.character(sp.layer$NAME10))
regionTag('KS2010',
	"Kansas county boundaries by US census, 2010",
	rLayerTag,
	mask
)

fourthRep <- c( 'Pawnee', 'Edwards', 'Kiowa', 'Comanche', 'Stafford',
	'Pratt', 'Barber', 'Kingman', 'Harper', 'Harvey', 'Sedgewick', 'Sumner',
	'Butler', 'Cowley', 'Greenwood', 'Elk', 'Chautauqua'
)
mask<-fuzzyMatch(fourthRep,as.character(sp.layer$NAME10))
regionTag('KS2010.4th',
	"Kansas 4th Representative by US census, 2010",
	rLayerTag,
	mask
)


rLayerTag <- 'UK.Unitary.Regions'
sp.layer <- get.Spatial(rLayerTag)
mask <- fuzzyMatch(rownames(elections$SIR2014),sp.layer$NAME)
regionTag('UK.Scotland.2014.SIR',
	'Scottish Independence Referendum, 2014',
	rLayerTag,
	mask
)

regionTag('UK.Northern.Ireland.Wm.5th',
	paste(sep=", ","United Kingdom","Northern Ireland",
		"Westminster","5th Review"
	),
	'NI.Wm.5th',
	fuzzyMatch(NINames,NI.Wm.5th.sp$PC_NAME)
)
