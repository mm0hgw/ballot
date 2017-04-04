#'get.nb
#'@inheritParams get.Spatial
#'@export
get.nb <- function(x){
#	cat(paste('get.nb',x,'\n'))
	UseMethod('get.nb',x)
}

#'get.nb.character
#'@method get.nb character
#'@inheritParams get.Spatial
#'@export
get.nb.character <- function(x){
	if(x %in% ls.ballotTag()){
		return(get.nb(as.ballotTag(x)))
	}
	if(x %in% ls.regionTag()){
		return(get.nb(as.regionTag(x)))
	}
	if(x %in% ls.layerTag()){
		return(get.nb(as.layerTag(x)))
	}
}

#'get.nb.ballotTag
#'@method get.nb ballotTag
#'@inheritParams get.Spatial
#'@export
get.nb.ballotTag <- function(x){
	get.nb(get.bRegionTag(x))
}

#'get.nb.regionTag
#'@method get.nb regionTag
#'@inheritParams get.Spatial
#'@export
get.nb.regionTag <- function(x){
#	cat(paste('get.nb.regionTag',x,'\n'))
	nb <- get.nb(get.rLayerTag(x))
	i <- get.rMask(x)
	out<-nb[i]
	return(out)
}

#'get.nb.layerTag
#'@importFrom spdep poly2nb
#'@method get.nb layerTag
#'@inheritParams get.Spatial
#'@export
get.nb.layerTag <- function(x){
#	cat(paste('get.nb.layerTag',x,'\n'))
	dataName <- paste(sep='', x, '.nb')
	fileName <- paste(sep='', layerDir, x, '.nb.rda')
	if(!file.exists(fileName)){
		sp <- get.Spatial(x)
		nb <- poly2nb(sp)
		textLinks <- get.textLinks(x)
		if (length(textLinks)>1){
			target <- sp@data[,get.linkSearchCol(x)]
			links <- findLinks(get.textLinks(x),target)
			nb <- nb.add(nb,links)
		}
		assign(dataName,nb)
		xvSave(list=dataName,
			file=fileName
		)
		return(nb)
	}else{
		load(file=fileName)
		return(get(dataName))
	}
}

findLinks <- function(textLinks,target){
	target <- as.character(target)
	len <- length(textLinks)
	stopifnot(len/2==len%/%2)
	stopifnot(len>0)
	out <- lapply(seq(len%/%2)*2,
		function(i){
			if(textLinks[i-1]%in%target && textLinks[i-1]%in%target){
				c(which.max(target==textLinks[i-1]),
					which.max(target==textLinks[i])
				)
			}else{
				NULL
			}
		}
	)
	out
}
