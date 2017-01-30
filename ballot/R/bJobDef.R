#' bJobDef
#'@param ballot a 'matrix' containing a ballot to process.
#'@param combo an 'ultraCombo' defining the combinations to process.
#'@param chunkSize an 'integer' defining the maximum number of combinations to process. Default is 2e4.
#'@param SBSORTFUN a 'function' which takes a sub-ballot matrix and returns a sortable value. Default is sbChisqTest.  
#'@param n an 'integer' the size of the head() to extract from the sorted SBSORTFUN results. Default is 1e3.
#'@param decreasing a 'logical' the order in which to sort SBSORTFUN results. Default is TRUE.
#'@param SBREPORTFUNS a 'list' of functions, each taking a sub ballot and returning a report value.
#' @param MCLAPPLYFUN a 'function' which performs lapply(). Default is the result of mclapplyFunGen() if it is installed, and lapply() otherwise.
#'@param .combine a 'function' used to combine the results generated.
#'@export
bJobDef <- function(
	ballot,
	combo,
	chunkSize=2e4,
	SBSORTFUN=sbChisqTest,
	n=1e3,
	decreasing=TRUE,
	SBFUN=sbReportGen(),
	MCLAPPLYFUN=get.lapply(),
	.combine=rbind
){
	is.valid.ballot(ballot)
	blankSortResults=lapply(rep(0,ncol(ballot)-1),invisible)
	out<-list(ballot=sortBallot(ballot),
		combo=combo,
		chunkSize=chunkSize,
		SBSORTFUN=SBSORTFUN,
		n=n,
		decreasing=decreasing,
		SBFUN=SBFUN,
		MCLAPPLYFUN=MCLAPPLYFUN	,
		.combine=.combine,
		sortCombos=new.env()
	)
	class(out)<-"bJobDef"
	out
}

#'run.bJobDef
#'@param x a 'bJobDef' object
#'@importFrom utils head
#'@method run bJobDef
#'@export
run.bJobDef <- function(x,...){
	sb_list<-splitBallot(x$ballot)
	out<-lapply(seq_along(sb_list),
		function(i){
			sb<-sb_list[[i]]
			comboName<-paste(sep="","c",i)
			if(exists(comboName,envir=x$sortCombos)){
				combo<-get(comboName,envir=x$sortCombos)
			}else{
				sortJob <- sbJobDef(
					sb=sb,
					combo=x$combo,
					chunkSize=x$chunkSize,
					SBFUN=x$SBSORTFUN,
					MCLAPPLYFUN=x$MCLAPPLYFUN,
					.combine=c
				)
				l<-run(sortJob)
				combo<-ultraCombo(
					head(n=x$n,
						x$combo$i[order(l,decreasing=x$decreasing)]
					),
					x$combo$n,
					x$combo$k
				)
				rm(l)
				assign(comboName,combo,envir=x$sortCombos)
			}
			reportJob <- sbJobDef(
				sb=sb,
				combo=combo,
				chunkSize=x$chunkSize,
				SBFUN=SBFUN,
				MCLAPPLYFUN=x$MCLAPPLYFUN,
				.combine=x$.combine
			)
			out<-run(reportJob)
			if(length(dim(out))!=0){
				if(ncol(out)==length(x$SBREPORTFUNS)){
					colnames(out)<-names(x$SBREPORTFUNS)
				}
				if(nrow(out)==combo$len){
					rownames(out)<-combo$i
				}
			}else{
				if(length(out)==combo$len){
					names(out)<-combo$i
				}
			}
			out
		}
	)
	names(out)<-names(sb_list)
	out
}
