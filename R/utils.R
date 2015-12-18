narrowFeatureData <- function(object,
                              n1 = 6, n2 = 6,
                              fcol = "markers") {
    if (length(fcol) > 1)
        fcol <- NULL
    if (is.null(fcol)) {
        i <- selectFeatureData(object)
        fData(object) <- fData(object)[, i]
        return(object)
    }
    n <- n1 + n2
    fv <- fvarLabels(object)
    ln <- length(fv)
    if (ln <= n) return(object)
    i <- c(1:n1, (ln-n2+1):ln)
    if (any(fcol %in% fv) & !any(fcol %in% fv[i]))
        i <- c(1:n1, (ln-n2+2):ln, match(fcol, fv))
    fData(object) <- fData(object)[, i]
    if (validObject(object))
        return(object)
}
