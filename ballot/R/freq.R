#'get.freq
#'@param x a 'ballotTag'
#'@param i an 'integer' index of sb
#'@export
get.freq <- function(x,i=1){
	cat(paste('get.freq',x,i,'\n'))
	x <- as.ballotTag(x)
	i <- as.integer(i)
	stopifnot(is.integer(i))
	stopifnot(length(i)==1)
	sbList <- splitBallot(get.ballot(x))
	sb <- sbList[[i]]
	sbName <- gsub(' ','.',names(sbList)[i])
	dataName <- paste(sep='',x,'_freq_',i,'_',sbName)
	fileName <- paste(sep='',ballotDir,dataName,'.rda')
	print(fileName)
	if(file.exists(fileName)){
		tmpEnv <- new.env()
		load(fileName,envir=tmpEnv)
		get(dataName,envir=tmpEnv)
	}else{
		tmpEnv <- new.env()
		freq <- reportAndCollate(x,
			sbPointsBelowPopMean,
			plyr::count
		)
		print(freq)
		assign(dataName, freq, envir=tmpEnv)
		save(list=dataName,file=fileName,envir=tmpEnv)
		freq
	}
}
