
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

get.chisq.ballotTag <- function(x,party='V'){
	dataFile <- paste( sep='', ballotDir, x,'.',party,'.chisq.rda')
	
	print(dataFile)
	print(party)
}
