#'layerTag
#'@inheritParams get.dsn
#'@inheritParams set.lTitle
#'@inheritParams set.refInfo
#'@inheritParams set.dsn
#'@inheritParams set.layer
#'@inheritParams set.textLinks
#'@inheritParams set.linkSearchCol
#'@export
layerTag <- function(x,
	lTitle=NULL,
	refInfo=NULL,
	dsn=NULL,
	layer=NULL,
	textLinks=NULL,
	linkSearchCol=NULL
){
#	cat('layerTag',x,'\n')
	stopifnot(is.valid.tag(x))
	if(!is.null(dsn))set.dsn(x,dsn)
	if(!is.null(lTitle))set.lTitle(x,lTitle)
	if(!is.null(refInfo))set.refInfo(x,refInfo)
	if(!is.null(layer))set.layer(x,layer)
	if(!is.null(linkSearchCol))set.linkSearchCol(x,linkSearchCol)
	if(!is.null(textLinks))set.textLinks(x,textLinks)
	stopifnot(exists.dsn(x))
	stopifnot(exists.lTitle(x))
	stopifnot(exists.refInfo(x))
	stopifnot(exists.linkSearchCol(x))
	stopifnot(exists.textLinks(x))	
	stopifnot(exists.layer(x))	
	out <- x
	class(out) <- c('layerTag','character')
	out
}

if(!exists('tmpEnv'))
	tmpEnv <- new.env()

#'@method format layerTag
format.layerTag <- function(x,...){
	paste(sep='',get.lTitle(x),
		'\nlayerTag ',x,' of shapefile layer ',get.layer(x),'\n'
	)
}

#'@method print layerTag
print.layerTag <- function(x,...){
	cat(format(x,...))
	invisible(x)
}

#'@importFrom sp plot
#'@method plot layerTag
plot.layerTag <- function(x,...){
	cat('plot.layerTag',x,'\n')
	sp <- get.Spatial(x)
	nb <- get.nb(x)
	sp::plot(sp,border="grey",main=get.lTitle(x))
	sp::plot(nb,coordinates(sp),col="red",add=TRUE)
}

#'is.layerTag
#'@param x an 'object' for testing
#'@export
is.layerTag <- function(x){
	inherits(x,'layerTag')
}

#'as.layerTag
#'@param x a 'character' or 'regionTag'
#'@export
as.layerTag <- function(x){
	if(length(x)>1)return(sapply(x,as.layerTag))
	if(is.layerTag(x))
		return(x)
	layerTag(x)
}

#'ls.layerTag
#'@param pattern a 'character' pattern to grep.
#'@export
ls.layerTag <- function(pattern=".*"){
	as.layerTag(
		grep(pattern,
			gsub('\\.layer$','',
				ls(envir=layerEnv,pattern='\\.layer$')
			),
			value=TRUE
		)
	)
}

#'ls.layers
#'@inheritParams get.dsn
#'@export
ls.layers <- function(x){
	if(exists.dsn(x)){
		dsn <- get.dsn(x)
		gsub(".shp$","",
			list.files(path=dsn,
				pattern=".shp"
			)
		)
	}else{
		stop(paste("no dsn found for",x))
	}
}

#'set.lTitle
#'@param lTitle a 'character' title
#'@inheritParams get.dsn
#'@export
set.lTitle <- function(x, lTitle){
	if(is.valid.tag(x) && is.valid.tag(lTitle)){
		if(exists.lTitle(x))
			if(get.lTitle(x)==lTitle)
				return(lTitle)
		x <- paste(sep='',x,".lTitle")
		assign(x, lTitle,envir=layerEnv)
		save.layerEnv()
		return(lTitle)
	}
	stop(paste('set.lTitle failed',x,lTitle))
}

#'set.dsn
#'@inheritParams get.dsn
#'@param dsn a 'character' path to shapefile
#'@export
set.dsn <- function(x,dsn){
	stopifnot(is.valid.tag(x))
	x <- paste(sep='',x,".dsn")
	stopifnot(is.character(dsn))
	stopifnot(length(list.files(path=dsn,pattern=".shp"))>0)
	if(exists.dsn(x))
		if(get.dsn(x)==dsn)
			return(dsn)
	assign(x,dsn,envir=layerEnv)
	save.layerEnv()
	return(dsn)
}

#'set.layer
#'@param layer a 'character' layer of shapefile
#'@inheritParams get.dsn
#'@export
set.layer <- function(x,layer){
	if(is.valid.tag(x) && 
		(layer %in% ls.layers(x))
	){
		if(exists.layer(x))
			if(get.layer(x)==layer)
				return(layer)
		x <- paste(sep='',x,".layer")
		assign(x,layer,envir=layerEnv)
		save.layerEnv()
		return(layer)
	}
	stop(paste('set.layer failed',x,layer))
}

#'set.refInfo
#'@param refInfo a 'character' roxygen2 @reference section 
#'@inheritParams get.dsn
#'@export
set.refInfo <- function(x, refInfo){
	if(is.valid.tag(x) && is.valid.tag(refInfo)){
		if(exists.refInfo(x))
			if(get.refInfo(x)==refInfo)
				return(refInfo)
		x <- paste(sep='',x,".refInfo")
		assign(x, refInfo,envir=layerEnv)
		save.layerEnv()
		return(refInfo)
	}
	stop(paste('set.refInfo failed',x,refInfo))
}

#'set.linkSearchCol
#'@inheritParams get.dsn
#'@param linkSearchCol a reference to the field of the shapefile referenced by #'@export
set.linkSearchCol <- function(x, linkSearchCol){
	if(is.valid.tag(x)){
		if(exists.linkSearchCol(x))
				if(get.linkSearchCol(x)==linkSearchCol)
					return(linkSearchCol)
		x <- paste(sep='',x,".linkSearchCol")
		assign(x, linkSearchCol,envir=layerEnv)
		save.layerEnv()
		return(linkSearchCol)
	}
	stop(paste('set.linkSearchCol failed',x,linkSearchCol))
}

#'set.textLinks
#'@param textLinks a 'character' vector defining links (ferries / bridges / etc) 
#'@inheritParams get.dsn
#'@export
set.textLinks <- function(x, textLinks){
	if(is.valid.tag(x) && is.character(textLinks)){
		if(length(textLinks)%%2==0 || textLinks==''){
			if(exists.textLinks(x))
				if(length(get.textLinks(x))==length(textLinks) &&
					all(get.textLinks(x)==textLinks)
				)
					return(textLinks)
			x <- paste(sep='',x,".textLinks")
			assign(x, textLinks,envir=layerEnv)
			save.layerEnv()
			return(textLinks)
		}
	}
	stop(paste('set.textLinks failed',x,textLinks))
}

#'get.refInfo
#'@inheritParams get.dsn
#'@export
get.refInfo <- function(x){
	x <- paste(sep='',x,".refInfo")
	get(x,envir=layerEnv)
}

#'get.linkSearchCol
#'@inheritParams get.dsn
#'@export
get.linkSearchCol <- function(x){
	x <- paste(sep='',x,".linkSearchCol")
	get(x,envir=layerEnv)
}

#'get.textLinks
#'@inheritParams get.dsn
#'@export
get.textLinks <- function(x){
	x <- paste(sep='',x,".textLinks")
	get(x,envir=layerEnv)
}

#'get.lTitle
#'@inheritParams get.dsn
#'@export
get.lTitle <- function(x){
	x <- paste(sep='',x,".lTitle")
	get(x,envir=layerEnv)
}

#'get.dsn
#'@param x a 'layerTag' 
#'@export
get.dsn <- function(x){
	x <- paste(sep='',x,".dsn")
	get(x,envir=layerEnv)
}

#'get.layer
#'@inheritParams get.dsn
#'@export
get.layer <- function(x){
	x <- paste(sep='',x,".layer")
	get(x,envir=layerEnv)
}

#'exists.dsn
#'@inheritParams get.dsn
#'@export
exists.dsn <- function(x){
	x <- paste(sep='',x,".dsn")
	exists(x,envir=layerEnv)
}

#'exists.layer
#'@inheritParams get.dsn
#'@export
exists.layer <- function(x){
	x <- paste(sep='',x,".layer")
	exists(x,envir=layerEnv)
}

#'exists.refInfo
#'@inheritParams get.dsn
#'@export
exists.refInfo <- function(x){
	x <- paste(sep='',x,".refInfo")
	exists(x,envir=layerEnv)
}

#'exists.linkSearchCol
#'@inheritParams get.dsn
#'@export
exists.linkSearchCol <- function(x){
	x <- paste(sep='',x,".linkSearchCol")
	exists(x,envir=layerEnv)
}

#'exists.lTitle
#'@inheritParams get.dsn
#'@export
exists.lTitle <- function(x){
	x <- paste(sep='',x,".lTitle")
	exists(x,envir=layerEnv)
}

#'exists.textLinks
#'@inheritParams get.dsn
#'@export
exists.textLinks <- function(x){
	x <- paste(sep='',x,".textLinks")
	exists(x,envir=layerEnv)
}

save.layerEnv <- function(){
	save(list=ls(layerEnv),
		file=layerFile,
		envir=layerEnv,
		compress='xz'
	)
}
