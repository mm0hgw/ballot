# Sub ballot processing routines

#' sbPopMean
#' Calculate the population mean of a sub-ballot
#' @param sb A sub-ballot
#' @return The population mean
#' @export
sbPopMean <- function(sb) {
    stopifnot(is.valid.subballot(sb))
    x <- colSums(sb)
    x[2]/x[1]
}

#' sbPopSd
#' Calculate the population standard deviation of a sub-ballot
#' @param sb A sub-ballot
#' @return The population standard deviation
#' @export
sbPopSd <- function(sb) {
    stopifnot(is.valid.subballot(sb))
    customSd(sb[, 2]/sb[, 1], center = sbPopMean(sb))
}

#' sbSampMean
#' Calculate the sample mean of a sub-ballot
#' @inheritParams sbPopMean
#' @return The sample mean
#' @export
sbSampMean <- function(sb) {
    stopifnot(is.valid.subballot(sb))
    mean(sb[2]/sb[1])
}

#' @title sbCalculateSample
#' @name calculateSample
#' @description Calculate sample from sub-ballot
#' @inheritParams sbPopMean
#' @param norm 'logical' value. Default FALSE. Whether sample is to be normalised.
#' @return a 'numeric' vector. The sample calculated from the sub-ballot
#' @export
sbCalculateSample <- function(sb, norm = FALSE) {
    stopifnot(is.valid.subballot(sb))
    stopifnot(is.logical(norm))
    if (norm == TRUE) {
        normalise(sb[, 2]/sb[, 1], center = sbPopMean(sb))
    } else {
        sb[, 2]/sb[, 1]
    }
}

#' sbChisqTest
#' @inheritParams sbPopMean
#' @param ... extra arguments passed on to density function.
#' @return chisq test result of sample of sub-ballot
#' @importFrom stats density dnorm
#' @export
sbChisqTest <- function(sb, ...) {
    stopifnot(is.valid.subballot(sb))
    d <- density(sbCalculateSample(sb, norm = TRUE), ...)
    y1 <- d$y
    y2 <- dnorm(d$x)
    sum((y1 - y2)^2)
}

#' sbPointsBelowPopMean
#' @inheritParams sbPopMean
#' @export
sbPointsBelowPopMean <- function(sb) {
    stopifnot(is.valid.subballot(sb))
    sum(sbCalculateSample(sb) < sbPopMean(sb))
}

#'sbDensity
#' @inheritParams sbPopMean 
#' @inheritParams sbChisqTest
#' @return x location of density peak of sample of sub-ballot
#' @importFrom stats density
#' @export
sbDensity <- function(sb, ...) {
    stopifnot(is.valid.subballot(sb))
    density(sbCalculateSample(sb, ...))
}

#'sbDensityPeakX
#' @inheritParams sbPopMean 
#' @inheritParams sbChisqTest
#' @return x location of density peak of sample of sub-ballot
#' @importFrom stats density
#' @export
sbDensityPeakX <- function(sb, ...) {
    stopifnot(is.valid.subballot(sb))
    d <- sbDensity(sb, ...)
    d$x[which.max(d$y)]
}

#'sbDensityPeakY
#' @inheritParams sbPopMean 
#' @inheritParams sbChisqTest
#' @return y location of density peak of sample of sub-ballot
#' @importFrom stats density
#' @export
sbDensityPeakY <- function(sb, ...) {
    stopifnot(is.valid.subballot(sb))
    d <- sbDensity(sb, ...)
    max(d$y)
}

#' sbSkewness
#' @inheritParams sbPopMean
#' @return skewness of sample of sub-ballot
#' @importFrom moments skewness
#' @export
sbSkewness <- function(sb) {
    stopifnot(is.valid.subballot(sb))
    moments::skewness(sbCalculateSample(sb))
}

#' sbKurtosis
#' @inheritParams sbPopMean
#' @return skewness of sample of sub-ballot
#' @importFrom moments kurtosis
#' @export
sbKurtosis <- function(sb) {
    stopifnot(is.valid.subballot(sb))
    moments::kurtosis(sbCalculateSample(sb))
}

#' sbDensityGen
#' @param norm 'logical' value. Default FALSE. Whether sample is to be normalised.
#' @param ... extra arguments for density()
#' @importFrom stats density
#' @importFrom graphics lines
#'@export
sbDensityGen <- function(norm = TRUE, ...) {
    stopifnot(is.logical(norm))
    function(sb) {
        stopifnot(is.valid.subballot(sb))
        density(sbCalculateSample(sb, norm = norm), ...)
    }
}

#'sbReportGen
#'@param SBREPORTFUNS a list of functions
#'@export
sbReportGen <- function(SBREPORTFUNS = list(chisqTest = sbChisqTest, popMean = sbPopMean, 
    popSd = sbPopSd, densityPeakX = sbDensityPeakX, densityPeakY = sbDensityPeakY, 
    skewness = sbSkewness, kurtosis = sbKurtosis)) {
    function(sb) {
        stopifnot(is.valid.subballot(sb))
        out <- sapply(SBREPORTFUNS, function(SBREPORTFUN) {
            SBREPORTFUN(sb)
        })
        names(out) <- names(SBREPORTFUNS)
        out
    }
}

#'sbSum
#'@inheritParams sbPopMean
#'@export
sbSum <- function(sb) {
    stopifnot(is.valid.subballot(sb))
    sum(sb[, 2])
}

#'sbSumN
#'@inheritParams sbPopMean
#'@export
sbSumN <- function(sb) {
    stopifnot(is.valid.subballot(sb))
    sum(sb[, 1])
}

#'sbAbstainers
#'@inheritParams sbPopMean
#'@export
sbAbstainers <- function(sb) {
    stopifnot(is.valid.subballot(sb))
    out <- sb[, seq(2)]
    out[, 2] <- sb[, 1] - sb[, 2]
    colnames(out)[2] <- gsub("^!!", "", gsub("^", "!", colnames(sb)[2]))
    out
}

#'sbBorgesiPlot
#'@inheritParams sbCalculateSample
#'@param ... extra arguments for plot function
#'@export
sbBorghesiPlot <- function(sb, norm = F, ...) {
    stopifnot(is.valid.subballot(sb))
    if (normalise == T) {
        pmean <- 0
        psd <- 1
    } else {
        pmean <- sbPopMean(sb)
        psd <- sbPopSd(sb)
    }
    d <- sbDensity(sb, norm = norm)
    model <- dnorm(d$x, mean = pmean, sd = psd)
    ylim <- range(model, d$y)
    plot(d, ylim = ylim, ...)
    lines(x = d$x, y = model, lty = 2)
    legend("topright", legend = c("measurement", "expectation"), lty = c(1, 2))
}

#'sbPnorm
#' @inheritParams sbPopMean
#'@importFrom stats pnorm
#'@export
sbPnorm <- function(sb) {
    mean <- sbPopMean(sb)
    sd <- sbPopSd(sb)
    function(...) {
        args <- list(...)
        args$mean <- mean
        args$sd <- sd
        do.call(pnorm, args)
    }
}

#'sbKsTest
#' @inheritParams sbPopMean
#'@param ... extra parameters for ks.test()
#'@importFrom stats ks.test
#'@export
sbKsTest <- function(sb, ...) {
    sample <- sbCalculateSample(sb)
    dist <- sbPnorm(sb)
    ks.test(sample, dist,...)
}
