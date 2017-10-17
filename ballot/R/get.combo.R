#'get.combo
#'@param x a '<foo>Tag' object
#'@export
get.combo <- function(x) {
    UseMethod("get.combo", x)
}

#'get.combo.character
#'@inheritParams get.combo
#'@method get.combo character
#'@export
get.combo.character <- function(x) {
    # cat(paste('get.combo.character',x,'\n'))
    get.combo(as.regionTag(x))
}

#'get.combo.ballotTag
#'@inheritParams get.combo
#'@method get.combo ballotTag
#'@export
get.combo.ballotTag <- function(x) {
    # cat(paste('get.combo.ballotTag',x,'\n'))
    get.combo(get.bRegionTag(x))
}

#'get.combo.regionTag
#'@method get.combo regionTag
#'@inheritParams get.combo
#'@export
get.combo.regionTag <- function(x) {
    # cat(paste('get.combo.regionTag',x,'\n'))
    tmpEnv <- new.env()
    dataName <- paste(sep = "", x, ".combo")
    fileName <- paste(sep = "", regionDir, x, ".combo.rda")
    if (!file.exists(fileName)) {
        nb <- get.nb(x)
        n <- length(nb)
        combo <- growCombo(nb)
        print(combo)
        assign(dataName, combo, envir = tmpEnv)
        xzSave(file = fileName, list = dataName, envir = tmpEnv)
    } else {
        load(file = fileName, envir = tmpEnv)
        combo <- get(dataName, envir = tmpEnv)
    }
    combo
}

#'growCombo
#'@importFrom ultraCombo ultraCombo comboChunk union.combo revCombnG
#'@importFrom get.lapply get.lapply get.chunkSize
#'@param nb an 'nb' object
#'@param k an 'integer' scalar. The k value to which to expand. Default is 7
#'@param seeds an 'integer' vector. The indices of the elements to expand. Default is all.
#'@export
growCombo <- function(nb, k = 7, seeds = 0) {
    k <- as.integer(k)
    seeds <- as.integer(seeds)
    cat(paste("nb length:", length(nb), "k", k, "seeds", paste(collapse = ",", seeds), 
        "\n"))
    stopifnot(inherits(nb, "nb"))
    stopifnot(is.integer(k))
    stopifnot(length(k) == 1)
    stopifnot(k >= 0)
    stopifnot(is.integer(seeds))
    stopifnot(sum(duplicated(seeds)) == 0)
    stopifnot(all(seeds == 0) || all(seeds > 0) || all(seeds < 0))
    n <- length(nb)
    if (k == 0) 
        return(ultraCombo(1, n, k))
    if (length(seeds) == 1 && seeds[1] == 0) 
        seeds <- seq(n)
    if (any(seeds < 0)) 
        seeds <- setdiff(seq(n), -seeds)
    combo <- ultraCombo(seeds, n, 1)
    LAPPLYFUN <- get.lapply()
    chunkSize <- get.chunkSize()
    while (combo$k < k) {
        combo <- do.call(union.combo, LAPPLYFUN(comboChunk(combo, chunkSize), function(combo) {
            i <- 1
            out <- ultraCombo(vector(), n, combo$k + 1)
            while (i <= combo$len) {
                x <- combo$Gen(i)
                out <- union.combo(out, ultraCombo::revCombnG(do.call(rbind, lapply(group.nb(nb, 
                  x), function(z) c(z, x))), n))
                print(out)
                i <- i + 1
            }
            out
        }))
        
    }
    combo$i <- sort(combo$i)
    combo
}
