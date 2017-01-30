#'[.nb
#'@param x target 'nb' object
#'@param i index vector
#'@importFrom spdep sym.attr.nb
#'@method [ nb
#'@export 
'[.nb' <- function(x, i) {
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
			i <- seq(n)[i]
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
	class(x) <- 'list'
	out<-lapply(x[i],
		function(y){
			y <- intersect(y,i)
			names(y) <- NULL
			out<-sapply(y,function(z)which.max(z==i))
			names(out)<-NULL
			if(length(out)==0){0}else{out}
		}
	)
	attr(out, "region.id") <- attr(x, "region.id")[i]
	xattrs <- names(attributes(x))
	for (j in 1:length(xattrs)) {
		if (xattrs[j] != "region.id")
			attr(out, xattrs[j]) <- attr(x, xattrs[j])
	}
	class(out) <- 'nb'
	spdep::sym.attr.nb(out)
}
