#'get.Spatial
#'@param x a '<foo>Tag' object or 'character' which is coerced to layerTag
#'@export
get.Spatial <- function(x){
#	cat(paste('get.Spatial',x,'\n'))
	UseMethod('get.Spatial',x)
}

#'get.Spatial.character
#'@method get.Spatial character
#'@inheritParams get.Spatial
#'@export
get.Spatial.character <- function(x){
#	cat(paste('get.Spatial.character',x,'\n'))
	if(x %in% ls.ballotTag()){
		return(get.Spatial(as.layerTag(x)))
	}
	if(x %in% ls.regionTag()){
		return(get.Spatial(as.regionTag(x)))
	}
	if(x %in% ls.layerTag()){
		return(get.Spatial(as.layerTag(x)))
	}
}

#'get.Spatial.ballotTag
#'@method get.Spatial ballotTag
#'@inheritParams get.Spatial
#'@export
get.Spatial.ballotTag <- function(x){
#	cat(paste('get.Spatial.ballotTag',x,'\n'))
	get.Spatial(get.bRegionTag(x))
}

#'get.Spatial.regionTag
#'@method get.Spatial regionTag
#'@import sp
#'@inheritParams get.Spatial
#'@export
get.Spatial.regionTag <- function(x){
#	cat(paste('get.Spatial.regionTag',x,'\n'))
	sp <- get.Spatial(get.rLayerTag(x))
	sp[get.rMask(x),]
}

#'get.Spatial.layerTag
#'@importFrom rgdal readOGR
#'@method get.Spatial layerTag
#'@inheritParams get.Spatial
#'@export
get.Spatial.layerTag <- function(x){
#	cat(paste('get.Spatial.layerTag',x,'\n'))
	if(exists.layer(x)){
		arg<-list(dsn=get.dsn(x),
			layer=get.layer(x)
		)
		spName <- paste(collapse=':',arg)
		if(exists(spName,envir=tmpEnv)){
			get(spName,envir=tmpEnv)
		}else{
			out <- do.call(rgdal::readOGR,arg)
			assign(spName, out, envir=tmpEnv )
			out
		}
	}else{
		stop(paste("no dsn found for",x))
	}
}
