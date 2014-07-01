## START unit test helperMinDist.R ##

## START unit test .minDistPCA ##
valuesPCA <- plot2D(andy2011, fcol=NULL, plot = FALSE)

test_.minDistPCA <- function() {
    checkEqualsNumeric(
        pRolocGUI:::.minDistPCA(-4.047683, 5.663844, 
                valuesPCA[, 1], valuesPCA[, 2], FALSE), 922)
    checkEquals(
        pRolocGUI:::.minDistPCA(-4.047683, 5.663844, 
                valuesPCA[, 1], valuesPCA[, 2], TRUE), "PARP1_HUMAN")
    checkTrue(
        is.null(pRolocGUI:::.minDistPCA(NULL, NULL, 
                valuesPCA[, 1], valuesPCA[, 2], TRUE)))
} 
## END unit test .minDistPCA ## 



## START unit test .minDistPlotDist ## 
test_.minDistPlotDist <- function() {
    checkEqualsNumeric(
        pRolocGUI:::.minDistPlotDist(list(andy2011, tan2009r1), 
            "all", "all", 3, 0.4957, "object1", FALSE), 47)
    checkEquals(
        pRolocGUI:::.minDistPlotDist(list(andy2011, tan2009r1), 
            "all", "all", 3, 0.4957, "object1", TRUE), "PGAP1_HUMAN")
    checkEqualsNumeric(
        pRolocGUI:::.minDistPlotDist(list(andy2011, tan2009r1), 
            "markers", "ER", 1, 0.27, "object1", FALSE), 29)
    checkEqualsNumeric(
        pRolocGUI:::.minDistPlotDist(list(andy2011, tan2009r1), 
            "all", "all", 4, 0.4435, "object2", FALSE), 383)
    checkEqualsNumeric(
        pRolocGUI:::.minDistPlotDist(list(andy2011, tan2009r1),
            "all", "all", 1, 0, "object2", TRUE), "FBgn0004584")
}
## END unit test .minDistPlotDist ## 



## END unit test helperMinDist.R ##
