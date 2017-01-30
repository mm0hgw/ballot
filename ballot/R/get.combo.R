#'get.combo
#'@param x a '<foo>Tag' object
#'@export
get.combo <- function(x){
	UseMethod('get.combo',x)
}

#'get.combo.ballotTag
#'@method get.combo ballotTag
#'@export
get.combo.ballotTag <- function(x){
	cat(paste('get.combo.ballotTag',x,'\n'))
	get.combo(get.bRegionTag(x))
}

#'get.combo.regionTag
#'@method get.combo regionTag
#'@inheritParams get.combo
#'@importFrom ultraCombo growCombo
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
		combo <- growCombo(x,nb)
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
