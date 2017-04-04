
xvSave <- function(...){
	arg <- list(...)
	arg$compress <- 'xz'
	arg$compression_level <- 9L
	do.call(save,arg)
}

xvConvert <- function(file){
	stopifnot(file.exists(file))
	if(strsplit(system(intern=TRUE,paste('file',file)),' ')[[1]][2]!='XZ'){
		print('[pop')
		tmpEnv <- new.env()
		load(envir=tmpEnv,file=file)
		xvSave(envir=tmpEnv,list=ls(tmpEnv),file=file)
	}
}
