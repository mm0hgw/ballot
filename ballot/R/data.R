
xzSave <- function(...) {
    arg <- list(...)
    arg$compress <- "xz"
    arg$compression_level <- 9L
    do.call(save, arg)
}

xzConvert <- function(file) {
    stopifnot(file.exists(file))
    if (strsplit(system(intern = TRUE, paste("file", file)), " ")[[1]][2] != "XZ") {
        print(file)
        tmpEnv <- new.env()
        load(envir = tmpEnv, file = file)
        xzSave(envir = tmpEnv, list = ls(tmpEnv), file = file)
    }
}
