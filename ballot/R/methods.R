# basic methods

#' run
#' @param x a Job definition object to run.
#' @export
run <-function(x,...){
	UseMethod("run",x)
}

#' @title Sample normaliser
#' @name normalise
#' @description Normalise a sample
#' @param x a 'numeric' vector. The sample to be normalised
#' @param center a 'numeric' the center to normalise to. length(center)==1
#' @return a 'numeric' vector. x normalised to center.
#' @export
normalise <- function(
	x,
	center=mean(x)
){
	if(length(center)!=1){
		print(center)
		stop("length of center != 1")
	}
 return(
 		( x - center ) / customSd( x, center )
 	)
}

#' @title Custom Standard Deviation
#' @name customSd
#' @description Standard deviation function with a parameter to adjust center.
#' @param x a 'numeric' vector. The sample
#' @param center a 'numeric' the center from which to calculate standard deviation. length(center)==1
#' @return a 'numeric' The standard deviation of the sample, measured from the center.
#' @export
customSd<-function(
	x,
	center=mean(x)
){
	if(length(center)!=1){
		print(center)
		stop("length of center != 1")
	}
 sqrt( mean( (x-center)^2 ) )
}

#' sortBallot
#' @description Sort ballot columns by descending total turnout
#' @param b ballot to sort
#' @export
sortBallot<-function(b){
	b[,order(colSums(b),decreasing=TRUE)]
}

#' splitBallot
#' @param ballot ballot to split
#' @return list of split ballots
#' @export
splitBallot <- function(
	ballot
){
	x<-	seq(ncol(ballot))[-1]
	sblist<-lapply(
		x,
		function(y){
			ballot[,c(1,y)]
		}
	)
	names(sblist)<-colnames(ballot)[x]
	sblist	
}

get.lapply <- function(){
	if(requireNamespace("mclapplyFunGen")){
		mclapplyFunGen::mclapplyFunGen()
	}else{
		lapply
	}
}
