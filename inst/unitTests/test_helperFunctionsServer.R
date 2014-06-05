## START unit test for helperFunctionsServer.R ## 



## START unit test .selClick ##
test_.selClick <- function() {
    checkEquals(.selClick(NULL, NULL, NULL, FALSE), NULL)
    checkEquals(.selClick("mousePCA", NULL, NULL, TRUE), NULL)
    checkEquals(.selClick("mousePlotDist", NULL, NULL, FALSE), NULL)
    checkEquals(.selClick(NULL, c(1, 1), NULL, TRUE), NULL)
    checkEquals(.selClick(NULL, c(1, 1), NULL, FALSE), NULL)
    checkEquals(.selClick(NULL, c(1, 1), 1, TRUE), "mousePCA")
    checkEquals(.selClick(NULL, c(1, 1), 1, FALSE), "mousePlotDist")
    checkEquals(.selClick(NULL, c(1, 1), c(1:10), TRUE), "mousePCA")
    checkEquals(.selClick(NULL, c(1, 1), c(1:10), FALSE), "mousePlotDist")
    checkEquals(.selClick("mousePCA", c(1, 1), NULL, TRUE), NULL)
    checkEquals(.selClick("mousePlotDist", c(1, 1), NULL), NULL)
    checkEquals(.selClick("mousePCA", c(1, 1), 1, TRUE), "mousePCA")
    checkEquals(.selClick("mousePlotDist", c(1, 1), 1, FALSE), "mousePlotDist")
}
## END unit test .selClick ##

## START unit test .selPlotDist ##
test_.selText <- function() {
    checkEquals(.selText(NULL, NULL, NULL, NULL), NULL)
    checkEquals(.selText(NULL, 0, NULL, NULL), NULL)
    checkEquals(.selText("text", 0, 1, 0), NULL)
    checkEquals(.selText(NULL, 0, 1, 0), NULL)
    checkEquals(.selText(NULL, 1, 0, 0), "text")
    checkEquals(.selText(NULL, 1, NULL, 1), "text")
    checkEquals(.selText(NULL, 1, 0, c(1:10)), "text")
    checkEquals(.selText(NULL, 2, 0, 0), "text")
    checkEquals(.selText("text", 1, 0, c(1:10)), "text")
}
## END unit test .selPlotDist ##

## START unit test .sI ##
test_.sI <- function() {
    checkEquals(.sI(NULL, NULL, NULL, NULL, NULL, NULL), NULL)
    checkEquals(.sI(NULL, NULL, 1:10, 1:10, 1:10, 1:10), NULL)
    checkEquals(.sI("text", NULL, 1:10, NULL, NULL, NULL), 1:10)
    checkEquals(.sI("mousePCA", NULL, NULL, 1:10, NULL, NULL), 1:10)
    checkEquals(.sI("mousePlotDist", NULL, NULL, NULL, 1:10, NULL), 1:10)
    checkEquals(.sI("savedSearches", NULL, NULL, NULL, NULL, 1:10), NULL)
    checkEquals(.sI("savedSearches", "x", NULL, NULL, NULL, 1:10), 1:10)
    checkEquals(.sI(c("text", "mousePCA"), NULL, 1:10, 1:10, NULL, NULL), 1:10)
    checkEquals(.sI(c("text", "mousePCA"), NULL, 1:10, 9:20, NULL, NULL), 1:20)
    checkEquals(.sI(c("text", "mousePCA"), "x", 1:10, 9:20, NULL, NULL), 1:20)
    checkEquals(.sI(c("text", "mousePCA", "savedSearches"), "x", 1:10, 
        9:20, NULL, NULL), 1:20)
    checkEquals(.sI(c("text", "savedSearches"), "x", 1:10, 9:20, 
        NULL, 100:105), c(1:10, 100:105))
    checkEquals(.sI(c("text", "mousePCA", "mousePlotDist", "savedSearches"), 
        "x", 1:10, 9:20, 50:60, 45:55), c(1:20, 45:60))
}
## END unit test .sI ##

## START unit test .sRsubset ##
test_.sRsubset <- function() {
    checkEquals(.sRsubset(andy2011, "protein", "ACA"), 
        c("ACAD9_HUMAN", "ACADV_HUMAN"))
    checkEquals(.sRsubset(andy2011, "markers", "E"), "ER")
    checkEquals(.sRsubset(andy2011, "markers", "ER"), "ER")
    checkEquals(.sRsubset(andy2011, "markers", ""), 
        levels(fData(andy2011)$markers))
    checkEquals(.sRsubset(andy2011, "pd.markers", ""), 
        levels(fData(andy2011)$pd.markers))
    checkEquals(.sRsubset(andy2011, "markers", "er"), character())
    checkEquals(.sRsubset(tan2009r1, "markers", "E"), "ER")
}
## END unit test .sRsubset ##

## START .obsProtText ##
test_.obsProtText <- function() {
    checkEquals(.obsProtText(NULL, NULL, NULL, NULL, NULL), NULL)
    checkEquals(.obsProtText(andy2011, NULL, NULL, NULL, NULL), NULL)
    checkEquals(.obsProtText(andy2011, 1:10, NULL, NULL, NULL), 1:10)
    checkEquals(.obsProtText(andy2011, 1:10, 1, NULL, NULL), 1:10)
    checkEquals(.obsProtText(andy2011, 1:10, 1, "ZPR1_HUMAN", NULL), 1:10)
    checkEquals(.obsProtText(andy2011, 1:10, 1, "ZPR1_HUMAN", "protein"), 
        c(1:10, 1371))
    checkEquals(.obsProtText(andy2011, 1:10, 1, "ER", "markers"), 1:80)
    checkEquals(.obsProtText(andy2011, 1:10, 0, "ER", "markers"), 1:10) 
    checkEquals(.obsProtText(tan2009r1, 1:10, NULL, NULL, NULL), 1:10)
    checkEquals(.obsProtText(tan2009r1, 1:10, 1, "Proteasome", "pd.markers"), 
        c(1:10, 14, 61, 104, 163, 213, 425, 435, 562, 734, 853))
}
## END unit test .obsProtText ##

## START unit test .obsProtClick ##
test_.obsProtClick <- function() {
    checkEquals(.obsProtClick(NULL, NULL, NULL), NULL)
    checkEquals(.obsProtClick(NULL, NULL, c(1, 1)), NULL)
    checkEquals(.obsProtClick(NULL, 10, c(1, 1)), 10)
    checkEquals(.obsProtClick(NULL, 10, NULL), NULL)
    checkEquals(.obsProtClick(NULL, 10, c(1, 1)), 10)
    checkEquals(.obsProtClick(1, 10, c(1, 1)), c(1, 10))
    checkEquals(.obsProtClick(1:10, 11, c(1, 1)), 1:11)
    checkEquals(.obsProtClick(1:10, NULL, c(1, 1)), 1:10)
    checkEquals(.obsProtClick(1:10, 11, NULL), 1:10)
    checkEquals(.obsProtClick(1:10, NULL, NULL), 1:10)
}
## END unit test .obsProtClick ##

## START unit test .fcex ##
test_.fcex <- function() {
    checkEquals(.fcex(andy2011), NULL)
    checkEquals(.fcex(tan2009r1), 
        c("No.peptide.IDs", "Mascot.score", "No.peptide.quantified"))
}
## END unit test .fcex ##

## START unit test .vPCA
test_.vPCA <- function() {
    checkEquals(.vPCA(NULL, NULL, NULL), NULL)
    checkEquals(.vPCA(andy2011, NULL, NULL), NULL)
    checkEquals(.vPCA(andy2011, 1, NULL), NULL)
    checkEquals(.vPCA(andy2011, NULL, 1), NULL)
    checkEquals(.vPCA(andy2011, 1, 2), plot2D(andy2011, plot = FALSE))
    checkEquals(.vPCA(andy2011, 3, 2), 
        plot2D(andy2011, dims = c(3, 2), plot = FALSE))
    checkEquals(.vPCA(tan2009r1, 1, 2), plot2D(tan2009r1, plot = FALSE))
}
## END unit test .vPCA

## START unit test .nC ##
test_.nC <- function() {
    checkEquals(.nC(NULL, "1"), 1)
    checkEquals(.nC(NULL, "2"), numeric())
    checkEquals(.nC(2, "1"), 1)
    checkEquals(.nC(1, "2"), 1)
    checkEquals(.nC(2, "2"), 2)
    checkEquals(.nC("1", "2"), 1)
    checkEquals(.nC("2", "2"), 2)
}
## END unit test .nC ##

## START unit test .orgName ##
test_.orgName <- function() {
    checkEquals(.orgName(NULL, NULL), NULL)
    checkEquals(.orgName(andy2011, NULL), NULL)
    checkEquals(.orgName(andy2011, "all"), "all")
    checkEquals(.orgName(andy2011, "markers"), 
        c("ER", "Golgi", "Mitochondrion", "PM", "unknown"))
    checkEquals(.orgName(tan2009r1, "all"), "all")
    checkEquals(.orgName(tan2009r1, "markers"),
        c("ER", "Golgi", "mitochondrion", "PM", "unknown"))
}
## END unit test .orgName ##

## START unit test .obsSavedSearch ##

## taken from Examples of help page of Features of Interest
x <- FeaturesOfInterest(
    description = "A traceable test set of features of interest",
    fnames = featureNames(tan2009r1)[1:10],
    object = tan2009r1)
## create FoICollection
xx <- FoICollection()

test_.obsSavedSearch <- function() {
    checkEquals(.obsSavedSearch(NULL, NULL, NULL, NULL, NULL), NULL)
    checkEquals(.obsSavedSearch(NULL, NULL, NULL, 1, NULL), NULL)
    checkEquals(.obsSavedSearch(xx, x, 1:10, 1, 
        "A traceable test set of features of interest"), 
        addFeaturesOfInterest(x, xx))
    checkEquals(.obsSavedSearch(xx, x, 1:10, 0, 
        "A traceable test set of features of interest"), xx)
    checkEquals(.obsSavedSearch(xx, x, NULL, 0, 
        "A traceable test set of features of interest"), xx)
}
## END unit test .obsSavedSearch ## 

## START unit test .whichTag ##
xx <- addFeaturesOfInterest(x, xx)
test_.whichTag <- function() {
    checkEquals(
        .whichTag("A traceable test set of features of interest", xx), 1)
}
## END unit test .whichTag ##

## START unit test .whichFOI ##
test_.whichFOI <- function() {
    checkEquals(.whichFOI(andy2011, xx, 1), integer())
    checkEquals(.whichFOI(tan2009r1, xx, 1), 1:10)
}
## END unit test .whichFOI

## START unit test .obsNewFoI ##
test_.obsNewFoI <- function() {
    checkEquals(.obsNewFoI(NULL, NULL, NULL, NULL), NULL)
    checkEquals(.obsNewFoI(andy2011, NULL, NULL, NULL), NULL)
    checkEquals(.obsNewFoI(NULL, 1:10, NULL, NULL), NULL)
    checkEquals(.obsNewFoI(NULL, NULL, "test", NULL), NULL)
    checkEquals(.obsNewFoI(andy2011, NULL, NULL, 1), NULL)
    checkEquals(.obsNewFoI(NULL, 1:10, NULL, 1), NULL)
    checkEquals(.obsNewFoI(NULL, NULL, "test", 1), NULL)
    checkEquals(.obsNewFoI(andy2011, 1:10, NULL, 1), NULL)
    checkEquals(.obsNewFoI(andy2011, NULL, "test", 1), NULL)
    checkEquals(foi(.obsNewFoI(andy2011, 1:10, "test", 1)), 
        foi(FeaturesOfInterest(description = "test", 
        fnames = featureNames(andy2011)[1:10], object = andy2011)))
    checkEquals(foi(.obsNewFoI(tan2009r1, 21:30, "test", 1)), 
        foi(FeaturesOfInterest(description = "test", 
        fnames = featureNames(tan2009r1)[21:30], object = tan2009r1)))
}

## END unit test .obsNewFoI ##

## START unit test for .fnamesFOI ## 

## taken from Examples of help page of Features of Interest
x <- FeaturesOfInterest(
    description = "A traceable test set of features of interest",
    fnames = featureNames(tan2009r1)[1:10],
    object = tan2009r1)

## create FoICollection
xx <- FoICollection()
xx <- addFeaturesOfInterest(x, xx)

## features to compare with
features <- c("FBgn0001104", "FBgn0000044", "FBgn0035720", "FBgn0003731", 
                "FBgn0029506", "FBgn0010638", "FBgn0028689", "FBgn0031871",
                "FBgn0040227", "FBgn0032799")
## create list
featuresList <- list(features)

test_fnamesFOI <- function() {
    checkEquals(pRolocGUI:::.fnamesFOI(x), features)
    checkEquals(pRolocGUI:::.fnamesFOI(xx), featuresList)
}
## END unit test for .fnamesFOI ## 

## 
## no unit test for .showFOI
##

## START manual unit test for .plotPCA ## 

## select MSnSet andy2011 
## plot in tab "PCA" has to look like:
    ## initially
    valuesPCA <- plot2D(andy2011, fcol=NULL, dims = c(1, 2))

    ## select "markers" in colour ("none" in symboltype, 
    ## PC 1 (along x-axis) and 2 (along y-axis), not zoomed)
    plot2D(andy2011, fcol = "markers", dims = c(1, 2), 
        xlim = c(min(valuesPCA[, 1]), max(valuesPCA[, 1])), 
        ylim = c(min(valuesPCA[, 2]), max(valuesPCA[, 2]))
    )

    ## select "pd.2013" in colour and "markers" in symboltype 
    ## (PC 1 (along x-axis) and 2 (along y-axis), not zoomed)
    plot2D(andy2011, fcol = "pd.2013", fpch = "markers", dims = c(1, 2), 
        xlim = c(min(valuesPCA[, 1]), max(valuesPCA[, 1])),
        ylim = c(min(valuesPCA[, 2]), max(valuesPCA[, 2]))
        )
    
    ## select "pd.markers" in colour and 
    ## PC 3 (along x-axis) and 4 (along y-axis) 
    ## ("none" in symboltype, not zoomed)
    plot2D(andy2011, fcol = "pd.markers", dims = c(3, 4),
        xlim = c(min(valuesPCA[, 1]), max(valuesPCA[, 1])), 
        ylim = c(min(valuesPCA[, 2]), max(valuesPCA[, 2]))
    )
    
    ## select "pd.markers" in colour and check box next to legend,  
    ## set position of legend to "topright"
    ## (PC 1 (along x-axis) and 2 (along y-axis), "none" in symboltype, 
    ## not zoomed)
    plot2D(andy2011, fcol = "pd.markers", dims = c(1, 2),
        xlim = c(min(valuesPCA[, 1]), max(valuesPCA[, 1])),
        ylim = c(min(valuesPCA[, 2]), max(valuesPCA[, 2]))
    )
    addLegend(andy2011, fcol = "pd.markers", where = "topright",
        bty = "n", cex = 1)

    ## select "pd.markers" in colour, PC 1 (along x-axis) and 2 (along y-axis) 
    ## and set zoom x-axis to (-2, 2) and zoom y-axis to (-3, 3)
    ## ("none" in symboltype)
    plot2D(andy2011, fcol = "pd.markers", dims = c(1, 2),
        xlim = c(-2, 2), ylim = c(-3, 3)
    )
    
    ## select MSnSet tan2009r1 and change to tab "PCA"
    ## select "pd.markers" in colour and "No.peptide.IDs"
    ## (PC 1 (along x-axis) and 2 (along y-axis), "none" in symboltype, 
    ## not zoomed)
    plot2D(tan2009r1, fcol = "pd.markers", 
        cex = fData(tan2009r1)[, "No.peptide.IDs"],
        dims = c(1, 2), 
        xlim = c(min(valuesPCA[, 1]), max(valuesPCA[, 1])),
        ylim = c(min(valuesPCA[, 2]), max(valuesPCA[, 2]))
    )
## END manual unit test for .plotPCA ##     



## START manual unit test for .plotPlotDist ## 
    
## select MSnSet andy2011
## plot in tab "protein profiles" has to look like:
    ## initially
    plotDist(andy2011)
    
    ## select "markers" in "feature(s) in" and "ER" in "assigned to"
    ## (number of plots to display 1)
    i <- which(fData(andy2011)$markers == "ER")
    plotDist(andy2011[i, ])

    ## select "pd.markers" in "feature(s) in" and "Lysosome" in "assigned to"
    ## (number of plots to display 1)
    i <- which(fData(andy2011)$pd.markers == "Lysosome")
    plotDist(andy2011[i, ])
    
    ## set number of plots to display to 4
    ## select "markers" in "feature(s) in" and "ER" in "assigned to"
    ## set "Selected plot" to 2
    ## select "markers" in "feature(s) in" and "Golgi" in "assigned to"
    ## set "Selected plot" to 3
    ## select "markers" in "feature(s) in" and "Mitochondrion" in "assigned to"
    ## set "Selected plot" to 4
    ## select "markers" in "feature(s) in" and "PM" in "assigned to"
    par(mfrow = c(2, 2))
    i <- which(fData(andy2011)$markers == "ER")
    plotDist(andy2011[i, ])
    i <- which(fData(andy2011)$markers == "Golgi")
    plotDist(andy2011[i, ])
    i <- which(fData(andy2011)$markers == "Mitochondrion")
    plotDist(andy2011[i, ])
    i <- which(fData(andy2011)$markers == "PM")
    plotDist(andy2011[i, ])
    par(mfrow = c(1, 1))
## END manuel unit test for .plotPlotDist ## 



## END unit test for helperFunctionsServer.R ## 
