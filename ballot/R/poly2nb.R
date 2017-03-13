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
		splitValue <- bbox[[splitCoord]][1]+deltas[[splitCoord]]/2

	}
	i.mask <- i.coords[,splitCoord]<splitValue
	j.mask <- j.coords[,splitCoord]<splitValue
	a.i.coords <- i.coords[i.mask,,drop=FALSE]
	b.i.coords <- i.coords[!i.mask,,drop=FALSE]
	a.j.coords <- j.coords[j.mask,,drop=FALSE]
	b.j.coords <- j.coords[!j.mask,,drop=FALSE]
	a<-list(i.coords=a.i.coords,j.coords=a.j.coords)
	b<-list(i.coords=b.i.coords,j.coords=b.j.coords)
	out<-list(a,b)
	print(summary(out))
	out
}

queenLinkFun <- function(i.coords,j.coords){
	if(length(i.coords)==0)return(FALSE)
	if(length(j.coords)==0)return(FALSE)
	i.coords<-matrix(i.coords,ncol=2)
	j.coords<-matrix(j.coords,ncol=2)

	coordsList <- list(set=list(i.coords,j.coords))

	while(length(coordsList)>0){
		print(summary(coordsList))
		n <- length(coordsList)
		pointCount <- sapply(coordsList[[n]],length)/2
		if(min(pointCount)==0){
			cat('dumping\n')
			coordsList[[n]] <- NULL
			next
		}
		if(min(pointCount)>10){
			cat('splitting\n')
			coordsList <- c(coordsList,do.call(splitCoords,coordsList[[n]]))
			coordsList[[n]] <- NULL
			next
		}
		cat('testing\n')
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

readPolyCoords <- function(sp,i){
	do.call(rbind,
		lapply(sp@polygons[[i]]@Polygons,
			function(i.poly)i.poly@coords
		)
	)
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
			i.coords <- readPolyCoords(sp,i)
			cat(paste('i',i,'points',length(i.coords),'\n'))
			j.seq <- seq(n)[-i]
			sapply(j.seq,
				function(j){
					j.coords <- readPolyCoords(sp,j)
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
