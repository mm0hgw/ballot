#' sbJobDef
#' @param sb a 'matrix' containing sub-ballot information
#' @param combo an 'ultraCombo' containing the desired combinations to process
#' @param chunkSize an 'integer' defining the largest job size to run. Default is 2e4.
#' @param SBFUN a 'function' which takes a sub-ballot and returns a result for sorting. Default is sbChisqTest().
#' @param MCLAPPLYFUN a 'function' which performs lapply(). Default is the result of mclapplyFunGen() if it is installed, and lapply() otherwise.
#'@export
sbJobDef <- function(
	sb,
	combo,
	chunkSize=2e4,
	SBFUN=sbChisqTest,
	MCLAPPLYFUN=get.lapply()
){
	out<-list(sb=sb,
		combo=combo,
		chunkSize=chunkSize,
		SBFUN=SBFUN,
		MCLAPPLYFUN=MCLAPPLYFUN
	)
	class(out)<-"sbJobDef"
	out
}

#'	runSbJobDef
#'@param sbJobDef a 'sbJobDef' job definition.
#'@importFrom ultraCombo ultraCombo chunk.combo
#'@export
runSbJobDef <- function(
	sbJobDef
){
	do.call(c,
		sbJobDef$MCLAPPLYFUN(chunk.combo(sbJobDef$combo,sbJobDef$chunkSize),
			function(combo){
				i<-1
				out<-vector()
				while(i<=combo$len){
					out<-c(out,sbJobDef$SBFUN(sbJobDef$sb[combo$Gen(i),]))
					i<-i+1
				}
				out
			}
		)
	)
}
