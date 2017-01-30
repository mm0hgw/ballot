load("test/data/elections.RData")

GE2010 <- elections$GE2010
Encoding(colnames(GE2010)) <- 'CP1252'
GE2010 <- GE2010[order(rownames(GE2010)),-1]
GE2010Names <- rownames(GE2010)
key10 <- as.factor(elections$GE2010[,1])
GE2015 <- elections$GE2015[,-1]
GE2015Names <- rownames(GE2015)
colnames(GE2015)[22]<-'Sinn Fein'
Encoding(GE2015Names) <- "CP1252"
GE2015Names <- iconv(GE2015Names, "CP1252", "UTF-8", sub='')
GE2015 <- GE2015[fuzzyMatch(GE2010Names,GE2015Names),]
rownames(GE2015) <- rownames(GE2010)

regionNames<-levels(key10)

GBNames<-setdiff(regionNames,"Northern Ireland")
NIkey <- key10 == "Northern Ireland"
NI2010 <- GE2010[NIkey,]
NI2015 <- GE2015[NIkey,]
			title <- paste(sep=", ",
				"United Kingdom",
				'Northern Ireland',
				"General Election",
				c(2010,2015)
			)
ballotTag('UK.Northern.Ireland.2010.Wm',NI2010,title[1],'UK.Northern.Ireland.Wm.5th')
ballotTag('UK.Northern.Ireland.2015.Wm',NI2015,title[2],'UK.Northern.Ireland.Wm.5th')

filenames <- gsub(" ",".",regionNames)
filenames <- gsub("&","and",filenames)

jobNames<-c(filenames,"GB")
jobRegion<-as.list(regionNames)
jobRegion[[grep("^GB$",jobNames)]] <- GBNames

contiguityNames <- paste(sep="","UK.",jobNames,".Wm.5th")
names2010 <- paste(sep="","UK.",jobNames,".2010.Wm")
names2015 <- paste(sep="","UK.",jobNames,".2015.Wm")

lapply(seq_along(jobNames),
	function(i){
		if(contiguityNames[i] %in% ls.regionTag()){
			regionName <- regionNames[i]
			if(is.na(regionName))
				regionName <- 'Great Britain'
			title <- paste(sep=", ",
				"United Kingdom",
				regionName,
				"General Election",
				c(2010,2015)
			)
			key <- key10 %in% jobRegion[[i]]
			ballotTag(names2010[i],
				ballot=GE2010[key,],
				bTitle=title[1],
				bRegionTag=contiguityNames[i]
			)
			ballotTag(names2015[i],
				ballot=GE2015[key,],
				bTitle=title[2],
				bRegionTag=contiguityNames[i]
			)
		}
	}
)
