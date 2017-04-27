if (!exists("layerEnv")) layerEnv <- new.env()
if (!exists("regionEnv")) regionEnv <- new.env()
if (!exists("ballotEnv")) ballotEnv <- new.env()

layerFile <- ".mm0hgw/layerEnv.rda"
regionFile <- ".mm0hgw/regionEnv.rda"
layerDir <- ".mm0hgw/layerTag/"
regionDir <- ".mm0hgw/regionTag/"
ballotFile <- ".mm0hgw/ballotEnv.rda"
ballotDir <- ".mm0hgw/ballotTag/"

.onLoad <- function(libname, pkgname) {
    if (!dir.exists(layerDir)) 
        dir.create(layerDir, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(regionDir)) 
        dir.create(regionDir, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(ballotDir)) 
        dir.create(ballotDir, recursive = TRUE, showWarnings = FALSE)
    xLoad(layerFile, envir = layerEnv)
    xLoad(regionFile, envir = regionEnv)
    xLoad(ballotFile, envir = ballotEnv)
}
