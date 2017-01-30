#'fuzzyMatch
#'@param a a 'character' vector to match
#'@param b a 'character' vector to be matched
#'@return an 'integer' vector defining the matched set in b
#'@importFrom plyr count
#'@export
fuzzyMatch <- function(a,b){
	a <- gsub(".","",a,fixed=TRUE)
	a <- gsub(",","",a,fixed=TRUE)
	a <- gsub("(","",a,fixed=TRUE)
	a <- gsub(")","",a,fixed=TRUE)
	a <- gsub("&","and",a,fixed=TRUE)
	b <- gsub(".","",b,fixed=TRUE)
	b <- gsub(",","",b,fixed=TRUE)
	b <- gsub("(","",b,fixed=TRUE)
	b <- gsub(")","",b,fixed=TRUE)
	b <- gsub("&","and",b,fixed=TRUE)
	
	n<-length(a)
	nseq <- seq(n)
	sapply(a,
		function(x){
			if(sum(x==b)==1)
				return(which.max(x==b))
			x2 <- strsplit(x," ")[[1]]
			g1 <- g2 <- lapply(x2,grep,b,ignore.case=TRUE)
			g1 <- setdiff(nseq,
				do.call(ultraCombo::multiUnion,
					lapply(g1,setdiff,x=nseq)
				)
			)
			if(length(g1)==1){
				return(g1)
			}
			if(length(g1)>1){
				b1 <- strsplit(b[g1]," ")
				b2 <- sapply(b1,length)
				if(sum(b2==length(x2))==1)
					return(g1[which.max(b2==length(x2))])
				g3 <- g1
			}else{
				c1 <- plyr::count(do.call(c,g2))
				m1 <- max(c1$freq)==c1$freq
				if(sum(m1)==1)
					return(c1$x[which.max(c1$freq)])
				if(sum(m1)>1){
					c2 <- c1$x[c1$freq==max(c1$freq)]
					c3 <- sapply(strsplit(b[c2]," "),length)
					m2 <- c3==length(x2)
					if(sum(m2)==1)
						return(c2[which.max(m2)])
				}
				g3 <- do.call(ultraCombo::multiUnion,g2)
				r1 <- sapply(g3,
					function(y){
						b1 <- strsplit(b[y]," ")[[1]]
						length(sapply(b1,grep,x))
					}
				)
				m1 <- max(r1)==r1
				if(sum(m1)==1)
					return(g3[m1])
			}
			out <- NULL
			while(is.null(out)){
				cat(
					paste(sep="\n",
						paste("Assessing",x),
						"Potential matches",
						paste(collapse="\n",
							paste(g3,":",b[g3])
						),
						""
					)
				)
				i <- as.integer(readline("Select number >"))
				cat("number",i,":",b[i],"selected to match",x,"\n")
				j <- readline("Confirm (y)? >")
				if(j=="y")
					out <- i
			}
			out
		}
	)
}
