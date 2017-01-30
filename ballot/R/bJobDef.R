#' bJobDef
#'@param ballot a 'matrix' containing a ballot to process.
#'@param combo an 'ultraCombo' defining the combinations to process.
#'@param chunkSize an 'integer' defining the maximum number of combinations to process. Default is 2e4.
#'@param SBSORTFUN a 'function' which takes a sub-ballot matrix and returns a sortable value. Default is sbChisqTest.  
#'@param n an 'integer' the size of the head() to extract from the sorted SBSORTFUN results. Default is 1e3.
#'@param decreasing a 'logical' the order in which to sort SBSORTFUN results. Default is TRUE.
#'@param SBREPORTFUNS a 'list' of functions, each taking a sub ballot and returning a report value.
#' @param MCLAPPLYFUN a 'function' which performs lapply(). Default is the result of mclapplyFunGen() if it is installed, and lapply() otherwise.
#'@export
bJobDef <- function(
	ballot,
	combo,
	chunkSize=2e4,
	SBSORTFUN=sbChisqTest,
	n=1e3,
	decreasing=TRUE,
	SBREPORTFUNS=list(
		chisq_test=sbChisqTest,
		pop_mean=sbPopMean,
		density_x=sbDensityPeakX,
		density_y=sbDensityPeakY,
		skewness=sbSkewness,
		kurtosis=sbKurtosis
	),
	MCLAPPLYFUN=get.lapply()	
){
	out<-list(ballot=ballot,
		combo=combo,
		chunkSize=chunkSize,
		SBSORTFUN=SBSORTFUN,
		n=n,
		decreasing=decreasing,
		SBREPORTFUNS=SBREPORTFUNS,
		MCLAPPLYFUN=MCLAPPLYFUN	
	)
	class(out)<-"bJobDef"
	out
}

#'runBJobDef
#'@param bJobDef a 'bJobDef' object
#'@importFrom utils head
#'@export
runBJobDef <- function(
	bJobDef
){
	lapply(splitBallot(bJobDef$ballot),
		function(sb){
			sortJob <- sbJobDef(
				sb=sb,
				combo=bJobDef$combo,
				chunkSize=bJobDef$chunkSize,
				SBFUN=bJobDef$SBSORTFUN,
				MCLAPPLYFUN=bJobDef$MCLAPPLYFUN	
			)
			l<-runSbJobDef(sortJob)
			combo<-ultraCombo(
				head(n=bJobDef$n,
					bJobDef$combo$i[order(l,decreasing=bJobDef$decreasing)]
				),
				bJobDef$combo$n,
				bJobDef$combo$k
			)
			rm(l)
			reportJob <- sbJobDef(
				sb=sb,
				combo=combo,
				chunkSize=bJobDef$chunkSize,
				SBFUN=function(sb){
					lapply(bJobDef$SBREPORTFUNS,
						function(SBREPORTFUN){
							SBREPORTFUN(sb)
						}
					)
				},
				MCLAPPLYFUN=bJobDef$MCLAPPLYFUN	
			)
			out<-matrix(runSbJobDef(reportJob),
				nrow=combo$len,
				byrow=TRUE
			)
			if(ncol(out)==length(bJobDef$SBREPORTFUNS)){
				colnames(out)<-names(bJobDef$SBREPORTFUNS)
			}
			rownames(out)<-combo$i
			out
		}
	)
}
