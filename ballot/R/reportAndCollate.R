#'reportAndCollate
#'@param x a 'ballotTag' object
#'@param SBREPORTFUN a 'function' that takes a sub-ballot matrix and returns a report
#'@param COLLATEFUN a 'function' that takes ... and returns collated results
#'@param ... extra arguments for SBREPORTFUN
#'@importFrom ultraCombo comboChunk
#'@export
reportAndCollate <- function(
	x,
	SBREPORTFUN,
	COLLATEFUN,
	...
){
	cat(paste('reportAndCollate',x,'\n'))
	stopifnot(is.function(SBREPORTFUN))
	stopifnot(is.function(COLLATEFUN))
	LAPPLYFUN <- get.lapply()
	chunkSize <- get.chunkSize()
	comboList <- ultraCombo::comboChunk(get.combo(x),chunkSize)
	z1 <- LAPPLYFUN(comboList,
		function(combo){
			lapply(seq(combo$len),
				function(i){
					SBREPORTFUN(get.ballot(x)[combo$Gen(i),],
						...
					)
				}
			)
		}
	)
	z2 <- do.call(c,z1)
	z3 <- do.call(COLLATEFUN,z2)
	z3
}
