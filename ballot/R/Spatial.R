#'spatialPlot
#'@param x a 'character' ballotTag
#'@param sample a 'numeric' vdctor with 1 element per electoral area.
#'@param ... extra plot parameters
#'@importFrom graphics plot image axis title par layout polygon
#'@importFrom sp plot
#'@export
spatialPlot <- function(x,sample=NULL,...){
	x <- as.ballotTag(x)
	arg <- list(...)
	if(!("main" %in% names(arg))){
		arg$main <- get.bTitle(x)
	}
	cat(paste("spatialPlot",x,"\n"))
	sp <- get.Spatial(x)
	arg$x <- sp
	if(is.null(sample)){
		sample <- sbCalculateSample(get.ballot(x),norm=TRUE)
	}
	color_vector <- sample_to_color(sample)
	lim<-max(abs(sample))
	leg<-seq(-lim,lim,length.out=256)
	layout(t(1:2),widths=c(6,1))
	par(mar=c(.5,.5,.5,.5),oma=rep(3,4),las=1)
	do.call(sp::plot,arg)
	lapply(seq_along(sample),
		function(i){
			p <- sp@polygons[[i]]
			q <- color_vector[i]
			lapply(p@Polygons,
				function(r)
					polygon(r@coords,col=q,border="grey")
			)
		}
	)
	image(1,
		leg,
		t(seq_along(leg)),
		col=sample_to_color(leg),
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
