#'regionTag
#'@param x a 'regionTag' or 'character'
#'@param rTitle a 'character' title
#'@param rLayerTag a 'layerTag' key
#'@param rMask a 'vector' defining the subset of the layerTag layer that forms the region.
#'@export
regionTag <- function(x,
	rTitle=NULL,
	rLayerTag=NULL,
	rMask=NULL
){
#	cat("regionTag",x,"\n")
	stopifnot(is.valid.tag(x))
	if(!is.null(rTitle))set.rTitle(x,rTitle)
	if(!is.null(rLayerTag))set.rLayerTag(x,rLayerTag)
	if(!is.null(rMask))set.rMask(x,rMask)
	stopifnot(exists.rTitle(x))
	stopifnot(exists.rLayerTag(x))
	stopifnot(exists.rMask(x))
	class(x) <- c('regionTag','character')
	x
}

#'@method format regionTag
format.regionTag <- function(x,...){
	paste(sep='',get.rTitle(x),
		'\nregionTag ',x,' of ',length(get.rMask(x)),' elements\n'
	)
}

#'@method print regionTag
print.regionTag <- function(x,...){
	cat(format(x,...))
	invisible(x)
}

#'@method plot regionTag
plot.regionTag <- function(x,...){
	
	sp <- get.Spatial(x)
	nb <- get.nb(x)
	sp::plot(sp,border="grey",main=get.rTitle(x))
	sp::plot(nb,coordinates(sp),col="red",add=TRUE)
}

#'is.regionTag
#'@param x an 'object' to test
#'@export
is.regionTag <- function(x){
	inherits(x,'regionTag')
}

#'as.regionTag
#'@param x a 'regionTag' or 'character'
#'@export
as.regionTag <- function(x){
	if(length(x)>1)return(sapply(x,as.regionTag))
	if(is.regionTag(x)){x}else{regionTag(x)}
}

#'ls.regionTag
#'@param pattern a 'character' pattern to grep.
#'@export
ls.regionTag <- function(pattern=".*"){
	as.regionTag(
		grep(pattern,
			gsub('\\.rLayerTag$','',
				ls(envir=regionEnv,pattern='\\.rLayerTag$')
			),
			value=TRUE
		)
	)
}

#'set.rTitle
#'@inheritParams get.rTitle
#'@param rTitle a 'character' title
#'@export
set.rTitle <- function(x, rTitle){
	if(is.valid.tag(x) && is.valid.tag(rTitle)){
		if(exists.rTitle(x))
			if(get.rTitle(x)==rTitle)
				return(rTitle)
		x <- paste(sep='',x,".rTitle")
		assign(x, rTitle,envir=regionEnv)
		save.regionEnv()
	}else{
		stop(paste('set.rTitle failed',x,rTitle))
	}
}

#'get.rTitle
#'@param x a 'regionTag' tag
#'@export
get.rTitle <- function(x){
	x <- paste(sep='',x,".rTitle")
	get(x,envir=regionEnv)
}

#'exists.rTitle
#'@inheritParams get.rTitle
#'@export
exists.rTitle <- function(x){
	x <- paste(sep='',x,".rTitle")
	exists(x,envir=regionEnv)
}

#'set.rLayerTag
#'@inheritParams get.rTitle
#'@param rLayerTag a 'layerTag' key
#'@export
set.rLayerTag <- function(x, rLayerTag){
	if(is.valid.tag(x)){
		rLayerTag <- as.layerTag(rLayerTag)
		if(exists.rLayerTag(x))
			if(get.rLayerTag(x)==rLayerTag &&
				length(class(get.rLayerTag(x)))==length(class(rLayerTag)) &&
				all(class(get.rLayerTag(x))==class(rLayerTag))
			)
				return(rLayerTag)
		x <- paste(sep='',x,".rLayerTag")
		assign(x, rLayerTag,envir=regionEnv)
		save.regionEnv()
		return(rLayerTag)
	}else{
		stop(paste('set.rLayerTag failed',x,rLayerTag))
	}
}

#'get.rLayerTag
#'@inheritParams get.rTitle
#'@export
get.rLayerTag <- function(x){
	x <- paste(sep='',x,".rLayerTag")
	get(x,envir=regionEnv)
}

#'exists.rLayerTag
#'@inheritParams get.rTitle
#'@export
exists.rLayerTag <- function(x){
	x <- paste(sep='',x,".rLayerTag")
	exists(x,envir=regionEnv)
}

#'set.rMask
#'@param rMask a 'vector' defining the subset of the layerTag layer that forms the region.
#'@inheritParams get.rTitle
#'@export
set.rMask <- function(x, rMask){
	if(is.valid.tag(x)){
		if(exists.rMask(x))
			if(length(get.rMask(x))==length(rMask) &&
				all(get.rMask(x)==rMask)
			)
				return(rMask)
		sp<-get.Spatial(get.rLayerTag(x))
		sp[rMask,]
		x <- paste(sep='',x,".rMask")
		assign(x, rMask,envir=regionEnv)
		save.regionEnv()
		return(rMask)
	}else{
		stop(paste('set.rLayerTag failed',x,paste(collapse=',',rMask)))
	}
}

#'get.rMask
#'@inheritParams get.rTitle
#'@export
get.rMask <- function(x){
	x <- paste(sep='',x,".rMask")
	get(x,envir=regionEnv)
}

#'exists.rMask
#'@inheritParams get.rTitle
#'@export
exists.rMask <- function(x){
	x <- paste(sep='',x,".rMask")
	exists(x,envir=regionEnv)
}

save.regionEnv <- function(){
	save(list=ls(regionEnv),
		file=regionFile,
		envir=regionEnv,
		compress='xz'
	)
}
