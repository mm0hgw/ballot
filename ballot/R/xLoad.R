#'xLoad
#'@description
#'Load on exist.
#'@param x a 'character' describing a file to attempt loading.
#'@param envir an 'environment' in which to load the specified file.
#'@export
xLoad <- function(x, envir = environment()) {
    if (file.exists(x)) {
        load(x, envir = envir)
    }
}
