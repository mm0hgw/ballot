
xvSave <- function(...){
	arg <- list(...)
	arg$compress <- 'xv'
	arg$compression_level <- 9L
	do.call(save,arg)
}
