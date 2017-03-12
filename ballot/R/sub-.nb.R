valid.nb.subset <- function(x,i){
	if(!inherits(x, 'nb'))
		stop('[.nb failed, supplied object not of class \'nb\'')
	n <- length(x)
	if(any(is.na(i)))
		stop('[.nb failed, NA indices supplied')
	if(is.character(i))
		stop(paste('[.nb failed, character indices',
				paste(collapse=',',i),'supplied and \'nb\' objects don\'t have names'
			)
		)
	if(is.logical(i))
		if(length(i)!=n)
			stop('[.nb failed, logical vector length does not match \'nb\' object length')
		else
			return(TRUE)
	if(all(i<0))
	  i <- -i
	if(any(errormask <- i < 1 || i > n))
		stop(paste('[.nb failed, indices',
				paste(collapse=',',i[errormask]),
				'supplied for \'nb\' object of length',n
			)
		)
	if(any(errormask <- duplicated(i)))
		stop(paste('[.nb failed, indices',
				paste(collapse=',',i[errormask]),
				'duplicated'
			)
		)
	return(TRUE)
}

#'[.nb
#'@param x target 'nb' object
#'@param i index vector
#'@importFrom spdep sym.attr.nb
#'@method [ nb
#'@export
'[.nb' <- function(x, i) {
	stopifnot(valid.nb.subset(x,i))
  n <- length(x)
	if(is.logical(i))
		i <- seq(length(x))[i]
	if(any(i<0))
	  i <- setdiff(seq(n),-i)
	class(x) <- 'list'
	names(x) <- NULL
	out<-lapply(x[i],
		function(y){
			out<-sapply(intersect(y,i),
				function(z)which.max(z==i)
			)
			if(length(out)==0){0L}else{out}
		}
	)
	attr(out, "region.id") <- attr(x, "region.id")[i]
	xattrs <- names(attributes(x))
	for (j in grep(invert=TRUE, value=TRUE, '^region.id$', xattrs)) {
		attr(out, j) <- attr(x, j)
	}
	class(out) <- 'nb'
	if(attr(out,'sym')) # if parent object was symmetric
		return(out) 		# child object must be symmetric.
	spdep::sym.attr.nb(out)
}
