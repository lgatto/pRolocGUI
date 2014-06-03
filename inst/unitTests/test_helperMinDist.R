## START unit test helperMinDist.R ##

## START unit test .minDistPCA ##
valuesPCA <- plot2D(andy2011, fcol=NULL, plot = FALSE)

test_minDistPCA <- function() {
    checkEqualsNumeric(
        .minDistPCA(-4.05, 5.65, valuesPCA[, 1], valuesPCA[, 2]), 
        922
    )
    checkEquals(
        names(.minDistPCA(-4.05, 5.65, valuesPCA[, 1], valuesPCA[, 2])), 
        "PARP1_HUMAN"
    )
}
## END unit test .minDistPCA ## 



## START unit test .minDistPlotDist ## 
test_minDistPlotDist <- function() {
    checkEqualsNumeric(
        .minDistPlotDist(andy2011, "all", "all", 3, 0.48), 
        47
    )
    checkEqualsNumeric(
        .minDistPlotDist(andy2011, "markers", "ER", 1, 0.27),
        29
    )
}
## END unit test .minDistPlotDist ## 



## END unit test helperMinDist.R ##