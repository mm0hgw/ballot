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

#'invade.nb
#'@description Given the context of an 'nb' object and a
#'subset of occupied territory, return the territory outwith
#'the input subset that borders the input subset.
#'@param nb 'nb' object
#'@param subset a 'vector' defining the currently occupied subset.
#'@importFrom sp plot
#'@export
invade.nb <- function(nb,subset){
	stopifnot(valid.nb.subset(nb,subset))
	if(is.logical(subset))
		subset <- seq(length(nb))[subset]
	class(nb) <- 'list'
	v1 <- unique(do.call(c,nb[subset]))
	v2 <- setdiff(v1,c(0,subset))
	sort(v2)
}

#'invade.nb.test
#'@inheritParams invade.nb
#'@export
invade.nb.test <- function(x,subset){
	sp <- get.Spatial(x)
	nb <- get.nb(x)
	stopifnot(valid.nb.subset(nb,subset))
	if(is.logical(subset))
		subset <- seq(length(nb))[subset]
	n <- length(nb)
	plot(sp)
	plot(sp[subset,],border=NA,col=1,add=TRUE)
	col <- 2
	while(length(subset)<n){
		invade <- invade.nb(nb,subset)
		plot(sp[invade,],border=NA,col=col,add=TRUE)
		subset <- union(subset,invade)
		col <- col + 1
	}
}

#'@importFrom dplyr between
splitCoords <- function(i.coords,j.coords){
	i.coords<-matrix(i.coords,ncol=2)
	j.coords<-matrix(j.coords,ncol=2)
	print(summary(i.coords))
	print(summary(j.coords))
	ni<-nrow(i.coords)
	nj<-nrow(j.coords)
	if(ni<nj){
		bbox <- lapply(seq(2),function(x)range(i.coords[,x]))
		deltas <- lapply(bbox,function(x){x[2]-x[1]})
		splitCoord <- which.max(deltas)
		splitValue <- c(bbox[[splitCoord]][1],
			bbox[[splitCoord]][1]+deltas[[splitCoord]]/2
		)
	}else{
		bbox <- lapply(seq(2),function(x)range(i.coords[,x]))
		deltas <- lapply(bbox,function(x){x[2]-x[1]})
		splitCoord <- which.max(deltas)
		splitValue <- c(bbox[[splitCoord]][1],
			bbox[[splitCoord]][1]+deltas[[splitCoord]]/2
		)
	}
	i.mask <- between(i.coords[,splitCoord],splitValue[1],splitValue[2])
	j.mask <- between(j.coords[,splitCoord],splitValue[1],splitValue[2])
	a.i.coords <- i.coords[i.mask,,drop=FALSE]
	b.i.coords <- i.coords[!i.mask,,drop=FALSE]
	a.j.coords <- j.coords[j.mask,,drop=FALSE]
	b.j.coords <- j.coords[!j.mask,,drop=FALSE]
	a<-list(i.coords=a.i.coords,j.coords=a.j.coords)
	b<-list(i.coords=b.i.coords,j.coords=b.j.coords)
	list(a,b)
}

#'@importFrom dplyr between
queenLinkFun <- function(i.coords,j.coords){
	if(length(i.coords)==0)return(FALSE)
	if(length(j.coords)==0)return(FALSE)
	i.coords<-matrix(i.coords,ncol=2)
	j.coords<-matrix(j.coords,ncol=2)
	ixr <- range(i.coords[,1])
	iyr <- range(i.coords[,2])
	jxr <- range(j.coords[,1])
	jyr <- range(j.coords[,2])
	xr <- c(max(ixr[1],jxr[1]),min(ixr[2],jxr[2]))
	if(xr[1]>xr[2])return(FALSE)
	yr <- c(max(iyr[1],jyr[1]),min(iyr[2],jyr[2]))
	if(yr[1]>yr[2])return(FALSE)

	i.coords <- i.coords[between(i.coords[,1],xr[1],xr[2]),]
	if(length(i.coords)==0)return(FALSE)
	i.coords <- i.coords[between(i.coords[,2],yr[1],yr[2]),]
	if(length(i.coords)==0)return(FALSE)
	print(length(i.coords))
	print(length(j.coords))
	j.coords <- j.coords[between(j.coords[,1],xr[1],xr[2]),]
	if(length(j.coords)==0)return(FALSE)
	j.coords <- j.coords[between(j.coords[,2],yr[1],yr[2]),]
	if(length(j.coords)==0)return(FALSE)
	print(length(j.coords))

	coordsList <- list(set=list(i.coords,j.coords))

	while(length(coordsList)>0){
		n <- length(coordsList)
		pointCount <- sapply(coordsList[[n]],length)/2
		if(min(pointCount)==0){
			coordsList[[n]] <- NULL
			next
		}
		if(min(pointCount)>10){
			coordsList <- c(coordsList,do.call(splitCoords,coordsList[[n]]))
			coordsList[[n]] <- NULL
			next
		}
		coordsA <- which.min(pointCount)
		coordsB <- setdiff(seq(2),coordsA)
		a.coords <- matrix(coordsList[[n]][[coordsA]],ncol=2)
		b.coords <- matrix(coordsList[[n]][[coordsB]],ncol=2)
		print(a.coords)
		print(b.coords)
		lapply(seq(min(pointCount)),
			function(a){
				lapply(seq(max(pointCount)),
					function(b){
						if(all(a.coords[a,] ==
							b.coords[b,]))
							return(TRUE)
					}
				)
			}
		)
		coordsList[[n]]<-NULL
	}

	return(FALSE)
}

#'poly2nb.funk
#'@param sp a 'SpatialPolygonsDataFrame' object
#'@param LINKFUN a 'function' to determine link status.
#'@export
poly2nb.funk <- function(sp,
	LINKFUN=queenLinkFun
){
	LAPPLYFUN <- get.lapply()
	n <- length(sp@polygons)
	lapply(seq(n),
		function(i){
			i.coords <- do.call(rbind,
				lapply(sp@polygons[[i]]@Polygons,
					function(i.poly)i.poly@coords
				)
			)
			cat(paste('i',i,'points',length(i.coords),'\n'))
			j.seq <- seq(n)[-i]
			sapply(j.seq,
				function(j){
					j.coords <- do.call(rbind,
						lapply(sp@polygons[[j]]@Polygons,
							function(j.poly)j.poly@coords
						)
					)
					cat(paste('i',i,'points',length(i.coords),'\n'))
					cat(paste('\tj',j,'points',length(j.coords),'\n'))
					if(LINKFUN(i.coords,j.coords)){
						j
					}else{
						vector()
					}
				}
			)
		}
	)
}
