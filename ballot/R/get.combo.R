#'get.combo
#'@param x a '<foo>Tag' object
#'@export
get.combo <- function(x){
	UseMethod('get.combo',x)
}

#'get.combo.ballotTag
#'@inheritParams get.combo
#'@method get.combo ballotTag
#'@export
get.combo.ballotTag <- function(x){
	cat(paste('get.combo.ballotTag',x,'\n'))
	get.combo(get.bRegionTag(x))
}

#'get.combo.regionTag
#'@method get.combo regionTag
#'@inheritParams get.combo
#'@export
get.combo.regionTag <- function(x){
	cat(paste('get.combo.regionTag',x,'\n'))
	comboEnv <- new.env()
	dataName <- paste(sep='', x, '.combo')
	fileName <- paste(sep='', regionDir, x, '.combo.rda')
	if(!file.exists(fileName)){
		nb <- get.nb(x)
		n <- length(nb)
		cat('nofile ',x,'\n')
		combo <- growCombo(nb)
		assign(dataName, combo, envir=comboEnv)
		save(file=fileName,envir=comboEnv,list=dataName)
	}else{
		cat('file\n')
		load(file=fileName,envir=comboEnv)
		combo <- get(dataName,envir=comboEnv)
	}
	print(combo)
	cat('get.combo end\n')
	combo
}

#'growCombo
#'@importFrom ultraCombo ultraCombo chunk.combo union.combo revCombnGG
#'@importFrom get.lapply get.lapply get.chunkSize
#'@export
growCombo <- function(nb,k=7,seeds=0){
	k <- as.integer(k)
	seeds <- as.integer(seeds)
	cat(paste('nb length:',length(nb),'k',k,'seeds',paste(collapse=',',seeds),'\n'))
	stopifnot(inherits(nb,'nb'))
	stopifnot(is.integer(k))
	stopifnot(length(k)==1)
	stopifnot(k>=0)
	stopifnot(is.integer(seeds))
	stopifnot(sum(duplicated(seeds))==0)
	stopifnot(all(seeds==0)||all(seeds>0)||all(seeds<0))
	n <- length(nb)
	cat(paste('n:',n,'\n'))
	if(k==0)
		return(ultraCombo(1,n,k))
	if(length(seeds)==1 && seeds[1]==0)
		seeds <- seq(n)
	cat(paste('seeds',paste(collapse=',',seeds),'\n'))
	combo <- ultraCombo(seeds,n,1)
	print(combo)
	revCombnGen <- revCombnGG(n)
	LAPPLYFUN <- get.lapply()
	chunkSize <- get.chunkSize()
	print(combo)
	while(combo$k < k){
		print(combo)
		combo <- do.call(union.combo,
			LAPPLYFUN(chunk.combo(combo,chunkSize),
				function(combo){
					i <- 1
					out <- ultraCombo(vector(),n,combo$k+1)
					while(i<=combo$len){
						x <- combo$Gen(i)
						out <- union.combo(out,
							revCombnGen(
								do.call(rbind,
									lapply(group.nb(nb,x),
										function(z)c(z,x)
									)
								)
							)
						)
						i <- i+1
					}
					out
				}
			)
		)
	}
	combo
}
