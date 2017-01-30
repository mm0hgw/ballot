#' sbJobDef
#' @param sb a 'matrix' containing sub-ballot information
#' @param combo an 'ultraCombo' containing the desired combinations to process
#' @param chunkSize an 'integer' defining the largest job size to run. Default is 2e4.
#' @param SBFUN a 'function' which takes a sub-ballot and returns a result for sorting. Default is sbChisqTest().
#' @param MCLAPPLYFUN a 'function' which performs lapply(). Default is the result of mclapplyFunGen() if it is installed, and lapply() otherwise.
#'@param .combine a 'function' used to combine the results generated.
#'@importFrom ultraCombo is.ultraCombo
#'@export
sbJobDef <- function(
	sb,
	combo,
	chunkSize=2e4,
	SBFUN=sbChisqTest,
	MCLAPPLYFUN=get.lapply(),
	.combine=c
){
	stopifnot(is.valid.subballot(sb))
	stopifnot(ultraCombo::is.ultraCombo(combo))
	if(chunkSize<1||any(chunkSize%%1!=0)){
		stop("bad chunkSize")
	}
	stopifnot(is.function(SBFUN))
	stopifnot(is.function(MCLAPPLYFUN))
	stopifnot(is.function(.combine))
	
	out<-list(sb=sb,
		combo=combo,
		chunkSize=chunkSize,
		SBFUN=SBFUN,
		MCLAPPLYFUN=MCLAPPLYFUN,
		.combine=.combine
	)
	class(out)<-"sbJobDef"
	out
}

#'	run.sbJobDef
#'@param x a 'sbJobDef' job definition.
#'@importFrom ultraCombo ultraCombo chunk.combo
#'@method run sbJobDef
#'@export
run.sbJobDef <- function(x,...){
	do.call(x$.combine,
		do.call(c,
			x$MCLAPPLYFUN(chunk.combo(x$combo,x$chunkSize),
				function(combo){
					i<-1
					out<-list()
					while(i<=combo$len){
						out<-c(out,x$SBFUN(x$sb[combo$Gen(i),]))
						i<-i+1
					}
					out
				}
			)
		)
	)
}
