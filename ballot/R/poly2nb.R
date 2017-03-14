splitCoords <- function(
	i.coords,
	j.coords,
	i,
	j
){
	if(length(i)<length(j)){
		bbox <- lapply(seq(2),function(x)range(i.coords[i,x]))
	}else{
		bbox <- lapply(seq(2),function(x)range(j.coords[j,x]))
	}
	deltas <- sapply(bbox,function(x){x[2]-x[1]})
	splitCoord <- which.max(deltas)
	splitValue <- bbox[[splitCoord]][1]+deltas[[splitCoord]]/2
	i.mask <- i.coords[i,splitCoord]<splitValue
	j.mask <- j.coords[j,splitCoord]<splitValue
	a.i <- i[i.mask]
	b.i <- i[!i.mask]
	a.j <- j[j.mask]
	b.j <- j[!j.mask]
	out<-list()
	out[[1]]<-list()
	out[[1]]$i<-a.i
	out[[1]]$j<-a.j
	out[[2]]<-list()
	out[[2]]$i<-b.i
	out[[2]]$j<-b.j
	print(summary(out))
	out
}

queenLinkFun <- function(i.coords,j.coords){
	i.coords<-matrix(i.coords,ncol=2)
	j.coords<-matrix(j.coords,ncol=2)

	coordsList <- list()
	coordsList[[1]] <- list(
		i=seq(nrow(i.coords)),
		j=seq(nrow(j.coords))
	)

	while(length(coordsList)>0){
		print(summary(coordsList))
		n <- length(coordsList)
		pointCount <- sapply(coordsList[[n]],length)
		if(min(pointCount)==0){
			cat('dumping\n')
			coordsList[[n]] <- NULL
			next
		}
		if(min(pointCount)>1){
			cat('splitting\n')
			coordsList <- c(coordsList,
				splitCoords(i.coords,
					j.coords,
					coordsList[[n]]$i,
					coordsList[[n]]$j
				)
			)
			coordsList[[n]] <- NULL
			next
		}
		cat('testing\n')
		x <- which.min(sapply(coordsList[[n]],length))
		if(x==1){
			if(i.coords[coordsList[[n]][[1]],] %in%
				j.coords[coordsList[[n]][[2]],] )
				return(TRUE)
		}else{
			if(j.coords[coordsList[[n]][[2]],] %in%
				i.coords[coordsList[[n]][[1]],] )
				return(TRUE)
		}
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
			cat(paste('i',i,'points',nrow(i.coords),'\n'))
			j.seq <- seq(n)[-i]
			sapply(j.seq,
				function(j){
					j.coords <- readPolyCoords(sp,j)
					cat(paste('i',i,'points',nrow(i.coords),'\n'))
					cat(paste('\tj',j,'points',nrow(j.coords),'\n'))
					if(queenLinkFun(i.coords,j.coords)){
						j
					}else{
						vector()
					}
				}
			)
		}
	)
}
