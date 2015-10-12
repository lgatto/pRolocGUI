getOrgThresholds <- function(object, fcol, scol, 
                             mcol = "markers", t = 0.5) {
  stopifnot(!missing(fcol))
  if (missing(scol)) 
    scol <- paste0(fcol, ".scores")
  object <- unknownMSnSet(object, mcol)
  nt <- tapply(fData(object)[, scol], fData(object)[, fcol], quantile, t)
  return(nt)
}

## Note: in tutorial LG used
# tapply(fData(object)$svm.scores, fData(object)$svm, median)
## this will include markers in calculating score thresholds
