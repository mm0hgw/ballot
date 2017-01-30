
#'is.valid.ballot
#'@description
#'Checks an object for validity as a matrix containing ballot information.
#'@param x an object to test
#'@export
is.valid.ballot <- function(x){
		# check for error states
	length(dim(x))==2 || stop("The number of the dimensions shall be two, and two shall be the number of the dimensions.")
	ncol(x)>=2 || stop("too few columns")
	!is.null(rownames(x)) || stop("rows not named")
	!is.null(colnames(x)) || stop("columns not named")
	colnames(x)[1] == "N" || stop("electorate (N) column not found")
	all(z<-x[,1]!=0) || stop(
		paste("0 detected as electorate in",
			paste(collapse=",",
				rownames(x)[z]
			)
		)
	)
		# check for warning states
	if(any(is.na(x)!=FALSE)){
		warning("NA values detected in ballot")
	}
	if(any(x<0)){
	 warning("negative values detected in ballot")
	}
	if(any(x%%1!=0)){
		warning("noninteger values detected in ballot")
	}
	if(colnames(x)[2] != "V"){
		warning("total vote count (V) column not found")
	}else{
		if(any(z<-x[,1]<x[,2])){
			warning(
				paste("V exceeds N in",
					paste(collapse=",",
						rownames(x)[z]
					)
				)
			)
		}
		sumW<-rowSums(x[TRUE,-seq(2),drop=FALSE])
		if(any(z<-x[,1]<sumW)){
			warning(
				paste("sum(W) exceeds N in",
					paste(collapse=",",
						rownames(x)[z]
					)
				)
			)
		}
		if(any(z<-x[,2]<sumW)){
			warning(
				paste("sum(W) exceeds V in",
					paste(collapse=",",
						rownames(x)[z]
					)
				)
			)
		}
	}
	return(TRUE)
}

is.valid.subballot <- function(x){
			# check for error states
	length(dim(x))==2 || stop("The number of the dimensions shall be two, and two shall be the number of the dimensions.")
	ncol(x)>=2 || stop("too few columns")
	!is.null(rownames(x)) || stop("rows not named")
	!is.null(colnames(x)) || stop("columns not named")
	colnames(x)[1] == "N" || stop("electorate (N) column not found")
	all(z<-x[,1]!=0) || stop(
		paste("0 detected as electorate in",
			paste(collapse=",",
				rownames(x)[z]
			)
		)
	)
		# check for warning states
	if(any(is.na(x)!=FALSE)){
		warning("NA values detected in ballot")
	}
	if(any(x<0)){
	 warning("negative values detected in ballot")
	}
	if(any(x%%1!=0)){
		warning("noninteger values detected in ballot")
	}
	if(any(z<-x[,1]<x[,2])){
		warning(
			paste(colnames(x)[2],
				"exceeds N in",
				paste(collapse=",",
					rownames(x)[z]
				)
			)
		)
	}
	return(TRUE)
}
