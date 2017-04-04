
xvSave <- function(...){
	arg <- list(...)
	arg$compress <- 'xv'
	arg$compression_level <- 9L
	do.call(save,arg)
}

xvConvert <- function(file){
	stopifnot(file.exists(file))
	if(strsplit(system(paste('file',file)),' ')[[1]][2]!='XV'){
		print('[pop')
		tmpEnv <- new.env()
	}
}
