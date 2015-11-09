
test_narrowFeatureData <- function() {
    library("pRolocdata")
    data(hyperLOPIT2015)
    fv <- fvarLabels(hyperLOPIT2015)
    n <- length(fv) ## 24
    k <- grep("markers", fv) ## 10

    ## default (not in default cols)
    x <- fvarLabels(pRolocGUI:::narrowFeatureData(hyperLOPIT2015, n1 = 6, n2 = 6))
    checkEqualsNumeric(length(x), 12)
    checkEquals(x, fv[c(1:6, 20:24, k)])
    ## smaller fdata (not in default cols)
    x <- fvarLabels(pRolocGUI:::narrowFeatureData(hyperLOPIT2015, n1 = 5, n2 = 5))
    checkEqualsNumeric(length(x), 10)
    checkEquals(x, fv[c(1:5, 21:24, k)])
    ## smaller fdata (not in default cols)
    x <- fvarLabels(pRolocGUI:::narrowFeatureData(hyperLOPIT2015, n1 = 3, n2 = 2))
    checkEqualsNumeric(length(x), 5)
    checkEquals(x, fv[c(1:3, 24, k)])
    ## all fdata
    x <- fvarLabels(pRolocGUI:::narrowFeatureData(hyperLOPIT2015, n1 = 24, n2 = 24))
    checkEqualsNumeric(length(x), n)
    checkEquals(x, fv)
    ## non-matching fcol
    x <- fvarLabels(pRolocGUI:::narrowFeatureData(hyperLOPIT2015, n1 = 6, n2 = 6, fcol = "nomatch"))
    checkEqualsNumeric(length(x), 12)
    checkEquals(x, fv[c(1:6, 19:24)])
    ## matching fcol in default selection
    x <- fvarLabels(pRolocGUI:::narrowFeatureData(hyperLOPIT2015, n1 = 6, n2 = 6, fcol = "EntryName"))
    checkEqualsNumeric(length(x), 12)
    checkEquals(x, fv[c(1:6, 19:24)])
}
