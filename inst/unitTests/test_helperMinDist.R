## START unit test helperMinDist.R ##

## START unit test .minDistPCA ##
valuesPCA <- plot2D(andy2011, fcol=NULL, plot = FALSE)

test_.minDistPCA <- function() {
    checkEqualsNumeric(
        pRolocGUI:::.minDistPCA(-4.047683, 5.663844, 
                valuesPCA[, 1], valuesPCA[, 2]), "P09874")
    checkEquals(
        pRolocGUI:::.minDistPCA(-4.4935615, -2.340266, 
                valuesPCA[, 1], valuesPCA[, 2]), "P49773")
    checkTrue(
        is.null(pRolocGUI:::.minDistPCA(NULL, NULL, 
                valuesPCA[, 1], valuesPCA[, 2])))
} 
## END unit test .minDistPCA ## 



## START unit test .minDistPlotDist ## 
test_.minDistPlotDist <- function() {
    checkEqualsNumeric(
        pRolocGUI:::.minDistPlotDist(list(andy2011, tan2009r1), 
            "all", "all", 3, 0.4957, "object1"), "Q75T13")
    checkEqualsNumeric(
        pRolocGUI:::.minDistPlotDist(list(andy2011, tan2009r1), 
            "markers", "ER", 1, 0.27, "object1"), "Q9H6H4")
    checkEqualsNumeric(
        pRolocGUI:::.minDistPlotDist(list(andy2011, tan2009r1), 
            "all", "all", 4, 0.4436, "object2"), c("Q9GU68", "Q86BR8"))
    checkEqualsNumeric(
        pRolocGUI:::.minDistPlotDist(list(andy2011, tan2009r1),
            "all", "all", 1, 0, "object2"), "P27864")
}
## END unit test .minDistPlotDist ## 



## END unit test helperMinDist.R ##
