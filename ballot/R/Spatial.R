#'spatialPlot
#'@param x a 'character' ballotTag
#'@param sample a 'numeric' vector with 1 element per electoral area.
#'@param ... extra plot parameters
#'@importFrom graphics image axis title par layout polygon
#'@importFrom sp plot
#'@export
spatialPlot <- function(x,sample=NULL,...){
	x <- as.ballotTag(x)
	arg <- list(...)
	if(!("main" %in% names(arg))){
		arg$main <- get.bTitle(x)
	}
#	cat(paste("spatialPlot",x,"\n"))
	sp <- get.Spatial(x)
	arg$x <- sp
	if(is.null(sample)){
		sample <- sbCalculateSample(get.ballot(x),norm=FALSE) * 100
	}
	color_vector <- sample_to_color(sample,n=4096)
	leg<-seq(min(sample),max(sample),length.out=256)
	layout(t(1:2),widths=c(6,1))
	par(mar=c(.5,.5,.5,.5),oma=rep(3,4),las=1)
	do.call(sp::plot,arg)
	lapply(seq_along(sample),
		function(i){
			p <- sp@polygons[[i]]
			q <- color_vector[i]
			lapply(p@Polygons,
				function(r)
					polygon(r@coords,col=q,border=NA)
			)
		}
	)
	image(1,
		leg,
		t(seq_along(leg)),
		col=sample_to_color(leg,n=4096),
		axes=FALSE,
		ylab="standard deviations from mean"
	)
	axis(4)
}

#'sample_to_color
#'@param sample a 'numeric' vector with 1 element per electoral area.
#'@param n number of colour bands
#'@importFrom fields two.colors
#'@export
sample_to_color <- function(sample,n=7){
	a <- max(abs(sample))
	pal <- two.colors(n=n,start='blue',end='red',middle='dark sea green')
	key <- floor((sample+a)*n/(2*a))+1
	key[key>n] <- n
	pal[key]
}

#'rainbowPlot
#'@param ... samples
#'@export
rainbowPlot<-function(...){
	l <- list(...)
	colLut<-lapply(l,function(sample,n){
			sample_to_color(sort(sample),n)
		},
		n=4096
	)
	ny <- length(l)
	nx <- length(l[[1]])
	sx <- seq(0,1,length.out=nx+1)
	sy <- seq(0,1,length.out=ny+1)
	plot(0,type='n',xlim=c(0,1),ylim=c(0,1))
	ix <- 1
	while(ix<=nx){
		iy <- 1
		while(iy<=ny){
			x <- c( sx[ix], sx[ix+1], sx[ix+1], sx[ix])
			y <- c( sy[iy], sy[iy], sy[iy+1], sy[iy+1])
			polygon(x,y,col=colLut[[iy]][ix],border=NA)
			iy <- iy + 1
		}
		ix <- ix + 1
	}
}
