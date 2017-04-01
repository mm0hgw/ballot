
#'get.chisq
#'@param x ballotTag
#'@export get.chisq
get.chisq <- function(x,party='V'){
	stopifnot(length(x)==1)
	UseMethod('get.chisq',x)
}

#'@method get.chisq character
get.chisq.character <- function(x,party='V'){
	x1 <- strsplit(x,' ')[[1]]
	x2 <- as.ballotTag(x1[1])
	if(length(x1>1)){
		party <- x1[2]
	}
	return(get.chisq(x2,party))
}

#'@method get.chisq ballotTag
get.chisq.ballotTag <- function(x,party='V'){
	dataName <- paste( sep='.', x, party, '.chisq')
	dataFile <- paste( sep='', ballotDir, dataName,'.rda')
	if(file.exists(dataFile)){
		load(dataFile)
	}else{
		LAPPLYFUN <- get.lapply::get.lapply()
		chunkSize <- get.lapply::get.chunkSize()
		chisq <- do.call(c,
			LAPPLYFUN(ultraCombo::comboChunk(z.sbChisqTest.dc,by=chunkSize),
				function(dc){
					sapply(seq(dc$len),dc$dGen)
				}
			)
		)
		assign(dataName,chisq)
		save(list=dataName,file=dataFile)
	}
	return(get(dataName))
}
