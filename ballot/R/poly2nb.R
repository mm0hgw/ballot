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
