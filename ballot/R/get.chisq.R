
#'get.chisq
#'@param x ballotTag
#'@export get.chisq
get.chisq <- function(x,party='V'){
	UseMethod('get.chisq',x)
}

#'@method get.chisq character
get.chisq.character <- function(x,party='V'){
	strsplit(x,' ')
}

get.chisq.ballotTag <- function(x,party='V'){
}
