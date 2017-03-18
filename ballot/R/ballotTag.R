#'ballotTag
#'@param x a 'character' containing a label.
#'@param ballot a 'matrix' defining ballot information.
#'@param bTitle a 'character' containing a title
#'@param bRegionTag a 'regionTag'
#'@export
ballotTag <- function(x,
	ballot=NULL,
	bTitle=NULL,
	bRegionTag=NULL
){
#	cat(paste('ballotTag',x,paste(collapse=':',class(x)),'\n'))
	stopifnot(is.valid.tag(x))
	if(!is.null(bRegionTag)){
		bRegionTag <- as.regionTag(bRegionTag)
		set.bRegionTag(x,bRegionTag)
	}
	if(!is.null(ballot))set.ballot(x,sortBallot(ballot))
	if(!is.null(bTitle))set.bTitle(x,bTitle)
	stopifnot(exists.ballot(x))
	stopifnot(exists.bTitle(x))
	stopifnot(exists.bRegionTag(x))
	out <- x
	class(out)<- c("ballotTag", "character")
	out
}

#'@method format ballotTag
format.ballotTag <- function(x,...){
	o<-paste(sep="\n\t","",get.bTitle(x),
		paste(rep("-",nchar(get.bTitle(x))),collapse="")
	)
	o<-c(o,paste("ballotTag object of",x))
	if(!is.null(get.bRegionTag(x)))
		o<-c(o,paste("based on",get.bRegionTag(x),"bRegionTag set"))
	o<-c(o,paste("covers",nrow(get.ballot(x)),"electoral areas"),
		""
	)
	paste(collapse="\n",o)
}

#'@method print ballotTag
print.ballotTag <- function(x,...){
	cat(format(x,...))
	invisible(x)
}

#'@importFrom get.lapply get.lapply get.chunkSize
#'@importFrom ultraCombo chunk.combo
#'@method reportAndCollate ballotTag
reportAndCollate.ballotTag <- function(
	x,
	SBREPORTFUN,
	COLLATEFUN,
	...
){
	LAPPLYFUN<-get.lapply()
	chunkSize <- get.chunkSize()
	sbList<-splitBallot(get.ballot(x))
	len<-length(sbList)
	out<-list()
	i<-1
	while(i<=len){
		sb<-sbList[[i]]
		a<-LAPPLYFUN(
			chunk.combo(
				get.combo(get.bRegionTag(x)),
				chunkSize
			),
			function(combo){
				j<-1
				out<-list()
				while(j<=combo$len){
					out<-c(out,SBREPORTFUN(sb[combo$Gen(j),],...))
					j<-j+1
				}
				out
			}
		)
		b<-do.call(c,a)
		out[[i]]<-do.call(COLLATEFUN,b)
		i<-i+1
	}
	names(out)<-names(sbList)
	out
}

#'is.ballotTag
#'@param x an 'object' for testing
#'@export
is.ballotTag <- function(x){
	inherits(x,'ballotTag')
}

#'as.ballotTag
#'@param x a 'character' or 'ballotTag'
#'@export
as.ballotTag <- function(x){
	if(length(x)>1)return(sapply(x,as.ballotTag))
	if(is.ballotTag(x))return(x)
	ballotTag(x)
}

#'@method plot ballotTag
#'@importFrom graphics legend lines
#'@importFrom sp plot
plot.ballotTag <- function(x,...){
#	cat(paste('plot.ballotTag',x,'\n'))
	arg<-densityArgList(...)
	if(!("main" %in% names(arg)))
		arg$main<-paste('Density Plot,',get.bTitle(x))
	norm<-TRUE
	if("norm" %in% names(arg)){
		norm<-arg$norm
		arg$norm<-NULL
	}
	slice <- seq(3)
	if('slice' %in% names(arg)){
		slice<-arg$slice
		arg$slice<-NULL
	}
	if(!("xlab" %in% names(arg))){
		ifelse(norm==TRUE,
			arg$xlab <- "Standard deviations from population mean",
			arg$xlab <- "Proportional turnout"
		)
	}
	sbList<-splitBallot(get.ballot(x))[slice]
	len<-length(sbList)
	if(length(grep('^V$',names(sbList)))>0){
		len <- len + 1
		sbList[['Abstainers']] <- sbList[['V']]
		sbList[['Abstainers']][,2] <- sbList[['V']][,1]-sbList[['V']][,2]
		sbList <- sbList[order(decreasing=TRUE,sapply(sbList,sbSum))]
	}
	dList<-lapply(sbList,function(x)sbDensity(x,norm=norm))
	if(!("xlim" %in% names(arg)))
		arg$xlim <- dListXlim(dList)
	if(!("ylim" %in% names(arg))){
		arg$ylim <- dListYlim(dList)
		arg$ylim[2] <- min(arg$ylim[2],
			max(1.5*c(max(dList[[1]]$y),dnorm(0)))
		)
	}
	do.call(sp::plot,arg)
	col<-seq(len)+1
	lapply(seq(len),
		function(x){lines(dList[[x]],col=col[x],lwd=3)}
	)
	leg<-paste(sapply(sbList,function(sb)format(big.mark=',',
				scientific=FALSE,
				sbSum(sb)
			)
		),
		gsub('^V$','Overall Turnout',names(sbList))
	)
	if(norm==TRUE){
		leg<-c(paste(format(big.mark=',',
					scientific=FALSE,
					sbSumN(sbList[[1]])
				),
				"Gaussian"
			),
			leg
		)
		col<-c(1,col)
		dx<-seq(arg$xlim[1],arg$xlim[2],length.out=512)
		lines(dx,dnorm(dx),lwd=3)
	}
	legend("topright",legend=leg,col=col,lwd=3)
}

#'ls.ballotTag
#'@param pattern a 'character' pattern to grep.
#'@export
ls.ballotTag <- function(pattern=".*"){
	as.ballotTag(
		grep(pattern,
			gsub('\\.ballot$','',
				ls(envir=ballotEnv,pattern='\\.ballot$')
			),
			value=TRUE
		)
	)
}

#'set.bTitle
#'@param bTitle a 'character' containing a title
#'@inheritParams get.bTitle
#'@export
set.bTitle <- function(x,bTitle){
	if(is.valid.tag(x) && is.valid.tag(bTitle)){
		x <- paste(sep='', x, '.bTitle')
		if(!exists(x,envir=ballotEnv) ||
			get(x,envir=ballotEnv)!=bTitle
		){
			assign(x,bTitle,envir=ballotEnv)
			save.ballotEnv()
		}
		return(bTitle)
	}
	stop(paste('set.bTitle failed',x,bTitle))
}

#'set.ballot
#'@param ballot a 'matrix' defining ballot information.
#'@inheritParams get.bTitle
#'@export
set.ballot <- function(x,ballot){
	if(!is.valid.tag(x))
		stop(paste(sep='', 'set.ballot bad tag',x))
	if(!is.valid.ballot(ballot,x)){
		print(ballot)
		stop(paste(sep='', 'set.ballot bad ballot'))
	}
	if(nrow(ballot)!=length(get.nb(get.bRegionTag(x))))
		stop('set.ballot ballot doesn\'t match region')
	x <- paste(sep='', x, '.ballot')
	assign(x,ballot,envir=ballotEnv)
	save.ballotEnv()
}

#'set.bRegionTag
#'@param bRegionTag a 'regionTag'
#'@inheritParams get.bTitle
#'@export
set.bRegionTag <- function(x,bRegionTag){
	bRegionTag <- as.regionTag(bRegionTag)
	print(bRegionTag)
	if(is.valid.tag(x) && inherits(bRegionTag,'regionTag')){
		x <- paste(sep='', x, '.bRegionTag')
		if(!exists(x,envir=ballotEnv) ||
			get(x,envir=ballotEnv)!=bRegionTag
		){
			assign(x,bRegionTag,envir=ballotEnv)
			save.ballotEnv()
		}
		return(bRegionTag)
	}
	stop(paste('set.bRegionTag failed',x,bRegionTag))
}

#'get.bTitle
#'@param x a 'ballotTag'
#'@export
get.bTitle <- function(x){
	x <- paste(sep='', x, '.bTitle')
	get(x,envir=ballotEnv)
}

#'get.ballot
#'@inheritParams get.bTitle
#'@export
get.ballot <- function(x){
	x <- paste(sep='', x, '.ballot')
	get(x,envir=ballotEnv)
}

#'get.bRegionTag
#'@inheritParams get.bTitle
#'@export
get.bRegionTag <- function(x){
	x <- paste(sep='', x, '.bRegionTag')
	get(x,envir=ballotEnv)
}

#'exists.bTitle
#'@inheritParams get.bTitle
#'@export
exists.bTitle <- function(x){
	x <- paste(sep='', x, '.bTitle')
	exists(x,envir=ballotEnv)
}

#'exists.ballot
#'@inheritParams get.bTitle
#'@export
exists.ballot <- function(x){
	x <- paste(sep='', x, '.ballot')
	exists(x,envir=ballotEnv)
}

#'exists.bRegionTag
#'@inheritParams get.bTitle
#'@export
exists.bRegionTag <- function(x){
	x <- paste(sep='', x, '.bRegionTag')
	exists(x,envir=ballotEnv)
}

save.ballotEnv <- function(){
	save(list=ls(ballotEnv),
		file=ballotFile,
		envir=ballotEnv,
		compress='xz'
	)
}
