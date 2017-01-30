#
#	Sub ballot processing routines
#

#' sbPopMean
#' Calculate the population mean of a sub-ballot
#' @param sb A sub-ballot
#' @return The population mean
#' @export
sbPopMean <- function(
	sb
){
	x<-colSums(sb)
	x[2]/x[1]
}

#' @title sbCalculateSample
#' @name calculateSample
#' @description Calculate sample from sub-ballot
#' @inheritParams sbPopMean
#' @param norm 'logical' value. Default FALSE. Whether sample is to be normalised.
#' @return a 'numeric' vector. The sample calculated from the sub-ballot
#' @export
sbCalculateSample<-function(
	sb,
	norm=FALSE
){
	if(norm){
		normalise(
			sb[,2]/sb[,1],
			center=sbPopMean(sb)
		)
	}else{
		sb[,2]/sb[,1]
	}
}

#' sbChisqTest
#' @inheritParams sbPopMean
#' @param ... extra arguments passed on to density function.
#' @return chisq test result of sample of sub-ballot
#' @importFrom stats density dnorm
#' @export
sbChisqTest <- function(
	sb,
	...
){
	d<-stats::density(
		sbCalculateSample(sb,norm=TRUE),
		...
	)
	y1<-d$y
	y2<-stats::dnorm(d$x)
	sum((y1-y2)^2)
}

#' sbCdfMeanIntercept
#' @inheritParams sbPopMean
#' @export
sbCdfMeanIntercept <- function(
	sb
){
	sum(sbCalculateSample(sb)>0) / nrow(sb)
}

#'	sbDensityPeakX
#' @inheritParams sbPopMean 
#' @inheritParams sbChisqTest
#' @return x location of density peak of sample of sub-ballot
#' @importFrom stats density
#' @export
sbDensityPeakX <- function(
	sb,
	...
){
	d<-stats::density(
		sbCalculateSample(sb),
		...
	)
	d$x[which.max(d$y)]
}

#'	sbDensityPeakY
#' @inheritParams sbPopMean 
#' @inheritParams sbChisqTest
#' @return y location of density peak of sample of sub-ballot
#' @importFrom stats density
#' @export
sbDensityPeakY <- function(
	sb,
	...
){
	d<-stats::density(
		sbCalculateSample(sb),
		...
	)
	max(d$y)
}

#' sbSkewness
#' @inheritParams sbPopMean
#' @return skewness of sample of sub-ballot
#' @importFrom moments skewness
#' @export
sbSkewness <- function(
	sb
){
 moments::skewness(sbCalculateSample(sb))
}

#' sbKurtosis
#' @inheritParams sbPopMean
#' @return skewness of sample of sub-ballot
#' @importFrom moments kurtosis
#' @export
sbKurtosis <- function(
	sb
){
	moments::kurtosis(sbCalculateSample(sb))
}
