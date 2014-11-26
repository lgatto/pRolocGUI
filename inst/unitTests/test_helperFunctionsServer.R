## START unit test for helperFunctionsServer.R ## 

##
## no unit test for .createSR()
##

## START unit test .namesObj ##
test_.namesObj <- function() {
    checkEquals(pRolocGUI:::.namesObj(NULL), character())
    checkEquals(pRolocGUI:::.namesObj(NULL, upload = TRUE), c("upload"))
    checkEquals(pRolocGUI:::.namesObj(1:10, name = "object"), c("object"))
    checkEquals(pRolocGUI:::.namesObj(1:10, name = "object", upload = TRUE), c("object", "upload"))
    checkEquals(pRolocGUI:::.namesObj(andy2011, name = "object"), c("object"))
    checkEquals(pRolocGUI:::.namesObj(list(NULL)), c("object1"))
    checkEquals(pRolocGUI:::.namesObj(list(andy2011)), c("object1"))
    checkEquals(pRolocGUI:::.namesObj(list(NULL, NULL), upload = TRUE), 
                                        c("object1", "object2", "upload"))
    checkEquals(pRolocGUI:::.namesObj(list(1:10, 2:11), upload = TRUE), 
                                        c("object1", "object2", "upload"))
    checkEquals(pRolocGUI:::.namesObj(list(andy2011, tan2009r1)), 
                                        c("object1", "object2"))
    checkEquals(pRolocGUI:::.namesObj(list(ut1 = andy2011, ut2 = tan2009r1)), 
                                        c("ut1", "ut2"))
    checkEquals(pRolocGUI:::.namesObj(list(andy2011, ut2 = tan2009r1), upload = TRUE), 
                                        c("object1", "ut2", "upload"))
}
## END unit test .namesObj ##



## START unit test .selClick ##
test_.selClick <- function() {
    checkEquals(pRolocGUI:::.selClick(NULL, NULL, NULL, FALSE, NULL), NULL)
    checkEquals(pRolocGUI:::.selClick("cursorPCA", NULL, NULL, TRUE, NULL), NULL)
    checkEquals(pRolocGUI:::.selClick("cursorPlotDist", 
                                    NULL, NULL, FALSE, NULL), NULL)
    checkEquals(pRolocGUI:::.selClick(NULL, c(1, 1), NULL, TRUE, NULL), NULL)
    checkEquals(pRolocGUI:::.selClick(NULL, c(1, 1), NULL, FALSE, NULL), NULL)
    checkEquals(
        pRolocGUI:::.selClick(NULL, c(1, 1), 1, TRUE, NULL), "cursorPCA")
    checkEquals(
        pRolocGUI:::.selClick(NULL, NULL, 1, TRUE, c(1, 1)), "cursorPCA")
    checkEquals(
        pRolocGUI:::.selClick(NULL, c(1, 1), 1, FALSE, NULL), "cursorPlotDist")
    checkEquals(
        pRolocGUI:::.selClick(NULL, NULL, 1, FALSE, c(1, 1)), "cursorPlotDist")
    checkEquals(
        pRolocGUI:::.selClick(NULL, c(1, 1), c(1:10), TRUE, NULL), "cursorPCA")
    checkEquals(pRolocGUI:::.selClick(NULL, 
                            c(1, 1), c(1:10), FALSE, NULL), "cursorPlotDist")
    checkEquals(
        pRolocGUI:::.selClick("cursorPCA", c(1, 1), NULL, TRUE, NULL), NULL)
    checkEquals(
        pRolocGUI:::.selClick("cursorPlotDist", c(1, 1), NULL, NULL), NULL)
    checkEquals(pRolocGUI:::.selClick("cursorPCA", 
                                    c(1, 1), 1, TRUE, NULL), "cursorPCA")
    checkEquals(pRolocGUI:::.selClick("cursorPlotDist", 
                                    c(1, 1), 1, FALSE, NULL), "cursorPlotDist")
}
## END unit test .selClick ##

## START unit test .selPlotDist ##
test_.selText <- function() {
    checkEquals(pRolocGUI:::.selButton(NULL, NULL, NULL, NULL), NULL)
    checkEquals(pRolocGUI:::.selButton(NULL, 0, NULL, NULL), NULL)
    checkEquals(pRolocGUI:::.selButton(NULL, 0, 1, 0), NULL)
    checkEquals(pRolocGUI:::.selButton("text", 0, 1, 0), "text")
    checkEquals(pRolocGUI:::.selButton(NULL, 1, 0, 0), "text")
    checkEquals(pRolocGUI:::.selButton(NULL, 1, NULL, 1), "text")
    checkEquals(pRolocGUI:::.selButton(NULL, 1, 0, c(1:10)), "text")
    checkEquals(pRolocGUI:::.selButton(NULL, 2, 0, 0), "text")
    checkEquals(pRolocGUI:::.selButton("text", 1, 0, c(1:10)), "text")
}
## END unit test .selPlotDist ##



## START unit test .computeInd ##
test_.computeInd <- function() {
    utList <- list(andy2011, tan2009r1)
    checkEquals(pRolocGUI:::.computeInd(list(NULL, NULL), NULL, "object1"), integer())
    checkEquals(pRolocGUI:::.computeInd(utList, NULL, "object1"), integer())
    checkEquals(pRolocGUI:::.computeInd(list(NULL, NULL), 
                                "XPO1_HUMAN", "object1"), integer())
    checkEquals(pRolocGUI:::.computeInd(utList, "XPO1_HUMAN", "object1"), 1355)
    checkEquals(pRolocGUI:::.computeInd(utList, 
                    c("XPO1_HUMAN", "ZN207_HUMAN"), "object1"), c(1355, 1364))
    checkEquals(pRolocGUI:::.computeInd(utList, 
                    "XPO1_HUMAN", "object2"), integer())
    checkEquals(pRolocGUI:::.computeInd(utList, 
        c("XPO1_HUMAN", "P41374", "Q24046"), "object2"), c(886, 843))
    checkEquals(pRolocGUI:::.computeInd(utList, 
            c("XPO1_HUMAN", "P41374", "Q24046"), "object1"), c(1355))
}
## END unit test .computeInd ##

## START unit test .sI ##
test_.sI <- function() {
    checkEquals(pRolocGUI:::.sI(NULL, NULL, NULL, NULL, NULL, NULL), NULL)
    checkEquals(pRolocGUI:::.sI(NULL, NULL, 1:10, 1:10, 1:10, 1:10), NULL)
    checkEquals(pRolocGUI:::.sI("text", NULL, 1:10, NULL, NULL, NULL), 1:10)
    checkEquals(pRolocGUI:::.sI("cursorPCA", NULL, NULL, 1:10, NULL, NULL), 
                1:10)
    checkEquals(pRolocGUI:::.sI("cursorPlotDist", NULL, NULL, NULL, 1:10, NULL), 
                1:10)
    checkEquals(pRolocGUI:::.sI("savedSearches", NULL, NULL, NULL, NULL, 1:10), 
                NULL)
    checkEquals(pRolocGUI:::.sI("savedSearches", "x", NULL, NULL, NULL, 1:10), 
                1:10)
    checkEquals(pRolocGUI:::.sI(c("text", "cursorPCA"),
                                NULL, 1:10, 1:10, NULL, NULL),
                1:10)
    
    checkEquals(pRolocGUI:::.sI(c("text", "cursorPCA"),
                                NULL, 1:10, 9:20, NULL, NULL),
                1:20)
    checkEquals(pRolocGUI:::.sI(c("text", "cursorPCA"),
                                "x", 1:10, 9:20, NULL, NULL),
                1:20)
    
    checkEquals(pRolocGUI:::.sI(c("text", "cursorPCA", "savedSearches"),
                                "x", 1:10, 9:20, NULL, NULL),
                1:20)
    checkEquals(pRolocGUI:::.sI(c("text", "savedSearches"),
                                "x", 1:10, 9:20, NULL, 100:105),
                c(1:10, 100:105))
    checkEquals(pRolocGUI:::.sI(c("text", "cursorPCA", "cursorPlotDist", 
                                  "savedSearches"), "x", 1:10, 9:20, 50:60, 45:55),
                c(1:20, 50:60, 45:49))
    checkEquals(pRolocGUI:::.sI(c("summat"), NULL, NULL, NULL, NULL, NULL, 1:10), 
                1:10)
}
## END unit test .sI ##

## START unit test .sIUni ##
test_.sIUni <- function() {
    checkEquals(pRolocGUI:::.sIUni(NULL, NULL), NULL)
    checkEquals(pRolocGUI:::.sIUni(NULL, "summat"), NULL)
    checkEquals(pRolocGUI:::.sIUni(1:10, NULL), NULL)
    checkEquals(pRolocGUI:::.sIUni(1:10, "summat"), 1:10)
}
## END unit test .sIUni ##

## START unit test .sRsubset ##
test_.sRsubset <- function() {
    utList <- list(andy2011, tan2009r1)
    checkEquals(pRolocGUI:::.sRsubset(utList, "protein", "ACA", "object1"), 
                                            c("ACAD9_HUMAN", "ACADV_HUMAN"))
    checkEquals(pRolocGUI:::.sRsubset(utList, "protein", "Q9W3M", 
                                "object2"), c("Q9W3M7", "Q9W3M8"))
    checkEquals(pRolocGUI:::.sRsubset(utList, "markers", "En", "object1"), "Endosome")
    checkEquals(pRolocGUI:::.sRsubset(utList, "markers", "ER", "object1"), "ER")
    checkEquals(sort(pRolocGUI:::.sRsubset(utList, "markers", "", "object2")), 
                                            sort(unique(fData(tan2009r1)$markers)))
    checkEquals(pRolocGUI:::.sRsubset(utList, "pd.markers", "", "object1"), 
                                            levels(fData(andy2011)$pd.markers))
    checkEquals(pRolocGUI:::.sRsubset(utList, "markers", "eri", "object2"), 
                                                                   character())
}
## END unit test .sRsubset ##



## START unit test .checkFeatText ##
test_.checkFeatText <- function() {
    utList <- list(andy2011, tan2009r1)
    checkTrue(!pRolocGUI:::.checkFeatText(utList, NULL, "ZPR1_HUMAN", 
                                            "protein", "object1", TRUE))
    checkTrue(pRolocGUI:::.checkFeatText(utList, "ZPR1_HUMAN", "ZPR1_HUMAN", 
                                            "protein", "object1", TRUE))
    checkTrue(!pRolocGUI:::.checkFeatText(utList, "ZPR1_HUMAN", "ZPR1_HUMAN", 
                                            "protein", "object1", FALSE))
    checkTrue(pRolocGUI:::.checkFeatText(utList, 1371, "ZPR1_HUMAN", 
                                            "protein", "object1", FALSE))
    checkTrue(!pRolocGUI:::.checkFeatText(utList, "ZPR1_HUMAN", "ER", 
                                            "markers", "object1", TRUE))
    checkTrue(pRolocGUI:::.checkFeatText(utList,
        c("BCLF1_HUMAN", "HNRPK_HUMAN", "IF4G1_HUMAN", "MAN1_HUMAN", 
          "NP1L4_HUMAN", "TCP4_HUMAN", "ZN787_HUMAN"), "Nucleus", 
        "pd.markers", "object1", TRUE))
    checkTrue(pRolocGUI:::.checkFeatText(utList, 882:888, "Q9V3V2", 
                                            "protein", "object2", FALSE))
    checkTrue(pRolocGUI:::.checkFeatText(utList, c("Q8SZM1", 
        "Q8MSI9"), "Q8MSI9", "protein", "object2", TRUE))
}
## END unit test .checkFeatText ##


## START .obsProtText ##
test_.obsProtText <- function() {
    utList <- list(andy2011, tan2009r1)
    checkEquals(pRolocGUI:::.obsProtText(NULL, NULL, NULL, NULL, NULL, 
                                                        "object1", FALSE), NULL)
    checkEquals(pRolocGUI:::.obsProtText(utList, NULL, NULL, NULL, NULL, 
                                                        "object1", FALSE), NULL)
    checkEquals(pRolocGUI:::.obsProtText(utList, 1:10, NULL, NULL, NULL, 
                                                        "object1", FALSE), NULL)
    checkEquals(pRolocGUI:::.obsProtText(utList, 1:10, 1, NULL, NULL, "object1", 
                                                                FALSE), 1:10)
    checkEquals(pRolocGUI:::.obsProtText(utList, 1:10, 1, NULL, NULL, "object2", 
                                                                FALSE), 1:10)
    checkEquals(pRolocGUI:::.obsProtText(utList, 1:10, 1, "ZPR1_HUMAN", NULL, 
                                                        "object1", FALSE), 1:10)
    checkEquals(pRolocGUI:::.obsProtText(utList, 1:10, 1, "ZPR1_HUMAN", 
                                    "protein", "object1", FALSE), c(1:10, 1371))
    checkEquals(pRolocGUI:::.obsProtText(utList, 1:10, 1, "Golgi", "markers", 
                                                        "object1", FALSE), 
                c(1:10, 82, 84:93, 98:103, 106, 611, 632, 790, 1147, 1212, 1282))
    checkEquals(pRolocGUI:::.obsProtText(utList, 1:10, 0, "ER", "markers", 
                                                        "object2", FALSE), 1:10) 
    checkEquals(pRolocGUI:::.obsProtText(utList, 1:10, 1, "Proteasome", 
        "pd.markers", "object2", FALSE), 
        c(1:10, 14, 61, 104, 163, 213, 425, 435, 562, 734, 853))
    checkEquals(pRolocGUI:::.obsProtText(utList, "ZPR1_HUMAN", 1, "ZNRF1_HUMAN", 
                    "protein", "object1", TRUE), c("ZPR1_HUMAN", "ZNRF1_HUMAN"))
}
## END unit test .obsProtText ##

## START unit test .removeFeat ##
test_.removeFeat <- function() {
    checkEquals(pRolocGUI:::.removeFeat(NULL, "a", 1), NULL)
    checkEquals(pRolocGUI:::.removeFeat(letters[1:10], "a", 0), letters[1:10])
    checkEquals(pRolocGUI:::.removeFeat(letters[1:10], "a", 1), letters[2:10])
    checkEquals(pRolocGUI:::.removeFeat(letters[1:10], c("a", "b"), 1), letters[3:10])
    checkEquals(pRolocGUI:::.removeFeat(letters[1:10], "z", 1), letters[1:10])
    
}

## END unit test .removeFeat ##


## START unit test .checkFeatData ##
test_.checkFeatData <- function() {
    checkEquals(pRolocGUI:::.checkFeatData(NULL, NULL, NULL, NULL, NULL), FALSE)
    checkEquals(
        pRolocGUI:::.checkFeatData(1:10, NULL, NULL, NULL, "common"), FALSE)
    checkEquals(
        pRolocGUI:::.checkFeatData(1:10, NULL, NULL, 5, "common"), TRUE)
    checkEquals(
        pRolocGUI:::.checkFeatData(1:10, NULL, NULL, 1:10, "common"), TRUE)
    checkEquals(
        pRolocGUI:::.checkFeatData(1:10, NULL, NULL, 1:11, "common"), FALSE)
    checkEquals(
        pRolocGUI:::.checkFeatData(NULL, 1:10, NULL, 12, "unique1"), FALSE)
    checkEquals(
        pRolocGUI:::.checkFeatData(NULL, 1:10, NULL, 1:10, "unique1"), TRUE)
    checkEquals(
        pRolocGUI:::.checkFeatData(NULL, 40:20, NULL, 12, "unique1"), FALSE)
    checkEquals(
        pRolocGUI:::.checkFeatData(NULL, 40:20, NULL, 22:23, "unique1"), TRUE)
    
}
## END unit test .checkFeatData ##

## START unit test .obsProtClick ##
test_.obsProtClick <- function() {
    checkEquals(pRolocGUI:::.obsProtClick(NULL, NULL, NULL), NULL)
    checkEquals(pRolocGUI:::.obsProtClick(NULL, NULL, c(1, 1)), NULL)
    checkEquals(pRolocGUI:::.obsProtClick(NULL, 10, c(1, 1)), 10)
    checkEquals(pRolocGUI:::.obsProtClick(NULL, 10, NULL), NULL)
    checkEquals(pRolocGUI:::.obsProtClick(NULL, 10, c(1, 1)), 10)
    checkEquals(pRolocGUI:::.obsProtClick(1, 10, c(1, 1)), c(1, 10))
    checkEquals(pRolocGUI:::.obsProtClick(1:10, 11, c(1, 1)), 1:11)
    checkEquals(pRolocGUI:::.obsProtClick(1:10, NULL, c(1, 1)), 1:10)
    checkEquals(pRolocGUI:::.obsProtClick(1:10, 11, NULL), 1:10)
    checkEquals(pRolocGUI:::.obsProtClick(1:10, NULL, NULL), 1:10)
}
## END unit test .obsProtClick ##



## START unit test .fcex ##
test_.fcex <- function() {
    checkEquals(pRolocGUI:::.fcex(andy2011), NULL)
    checkEquals(pRolocGUI:::.fcex(tan2009r1), 
        c("No.peptide.IDs", "Mascot.score", "No.peptide.quantified"))
}
## END unit test .fcex ##

## START unit test .vPCA
test_.vPCA <- function() {
    utList <- list(andy2011, tan2009r1)
    mirX <- -plot2D(andy2011, plot = FALSE)[, 1]
    mirY <- -plot2D(andy2011, plot = FALSE)[, 2]
    checkEquals(pRolocGUI:::.vPCA(NULL, NULL, NULL, "object1", FALSE, FALSE), NULL)
    checkEquals(pRolocGUI:::.vPCA(utList, NULL, NULL, "object1", FALSE, FALSE), NULL)
    checkEquals(pRolocGUI:::.vPCA(utList, 1, NULL, "object1", FALSE, FALSE), NULL)
    checkEquals(pRolocGUI:::.vPCA(utList, NULL, 1, "object1", FALSE, FALSE), NULL)
    checkEquals(pRolocGUI:::.vPCA(utList, 1, 2, "object1", FALSE, FALSE), 
                    plot2D(andy2011, plot = FALSE))
    checkEquals(pRolocGUI:::.vPCA(utList, 1, 2, "object1", TRUE, FALSE)[, 1], 
                mirX)
    checkEquals(pRolocGUI:::.vPCA(utList, 1, 2, "object1", FALSE, TRUE)[, 2],
                mirY)
    checkEquals(pRolocGUI:::.vPCA(utList, 1, 2, "object1", TRUE, TRUE)[, 1], 
                mirX)
    checkEquals(pRolocGUI:::.vPCA(utList, 1, 2, "object1", TRUE, TRUE)[, 2],
                mirY)
    checkEquals(pRolocGUI:::.vPCA(utList, 3, 2, "object1", FALSE, FALSE), 
                            plot2D(andy2011, dims = c(3, 2), plot = FALSE))
    checkEquals(pRolocGUI:::.vPCA(utList, 1, 2, "object2", FALSE, FALSE), 
                                            plot2D(tan2009r1, plot = FALSE))
}
## END unit test .vPCA



## START unit test .nC ##
test_.nC <- function() {
    checkEquals(pRolocGUI:::.nC(NULL, "1"), 1)
    checkEquals(pRolocGUI:::.nC(NULL, "2"), numeric())
    checkEquals(pRolocGUI:::.nC(2, "1"), 1)
    checkEquals(pRolocGUI:::.nC(1, "2"), 1)
    checkEquals(pRolocGUI:::.nC(2, "2"), 2)
    checkEquals(pRolocGUI:::.nC("1", "2"), 1)
    checkEquals(pRolocGUI:::.nC("2", "2"), 2)
}
## END unit test .nC ##

## START unit test .orgName ##
test_.orgName <- function() {
    utList <- list(andy2011, tan2009r1)
    checkEquals(pRolocGUI:::.orgName(NULL, NULL), NULL)
    checkEquals(pRolocGUI:::.orgName(utList, NULL, "object1"), NULL)
    checkEquals(pRolocGUI:::.orgName(utList, "all", "object1"), "all")
    checkEquals(pRolocGUI:::.orgName(utList, "markers", "object1"), 
        sort(unique(fData(andy2011)$markers)))
    checkEquals(pRolocGUI:::.orgName(utList, "all", "object2"), "all")
    checkEquals(pRolocGUI:::.orgName(utList, "markers", "object2"),
        sort(unique(fData(tan2009r1)$markers)))
}
## END unit test .orgName ##

## START unit test .obsSavedSearch ##
test_.obsSavedSearch <- function() {
    ## taken from Examples of help page of Features of Interest
    x <- FeaturesOfInterest(
        description = "A traceable test set of features of interest",
        fnames = featureNames(tan2009r1)[1:10],
        object = tan2009r1)
    ## create FoICollection
    xx <- FoICollection()
    checkEquals(pRolocGUI:::.obsSavedSearch(NULL, NULL, NULL, NULL, NULL), NULL)
    checkEquals(pRolocGUI:::.obsSavedSearch(NULL, NULL, NULL, 1, NULL), NULL)
    checkEquals(pRolocGUI:::.obsSavedSearch(xx, x, 1:10, 1, 
        "A traceable test set of features of interest"), 
        addFeaturesOfInterest(x, xx))
    checkEquals(pRolocGUI:::.obsSavedSearch(xx, x, 1:10, 0, 
        "A traceable test set of features of interest"), xx)
    checkEquals(pRolocGUI:::.obsSavedSearch(xx, x, NULL, 0, 
        "A traceable test set of features of interest"), xx)
}
## END unit test .obsSavedSearch ## 

## START unit test .whichTag ##
test_.whichTag <- function() {
    ## taken from Examples of help page of Features of Interest
    x <- FeaturesOfInterest(
        description = "A traceable test set of features of interest",
        fnames = featureNames(tan2009r1)[1:10],
        object = tan2009r1)
    ## create FoICollection
    xx <- FoICollection()    
    xx <- addFeaturesOfInterest(x, xx)
    checkEquals(pRolocGUI:::.whichTag(
        "A traceable test set of features of interest", xx), 1)
    checkTrue(is.na(pRolocGUI:::.whichTag("not existing", xx)))
}
## END unit test .whichTag ##

## START unit test .whichFOI ##
test_.whichFOI <- function() {
    utList <- list(andy2011, tan2009r1)
    ## taken from Examples of help page of Features of Interest
    x <- FeaturesOfInterest(
        description = "A traceable test set of features of interest",
        fnames = featureNames(tan2009r1)[1:10],
        object = tan2009r1)
    ## create FoICollection
    xx <- FoICollection()    
    xx <- addFeaturesOfInterest(x, xx)
    checkEquals(pRolocGUI:::.whichFOI(utList, xx, 1, "object1"), integer())
    checkEquals(pRolocGUI:::.whichFOI(utList, xx, 1, "object2"), 1:10)
}
## END unit test .whichFOI

## START unit test .obsNewFoI ##
test_.obsNewFoI <- function() {
    utList <- list(andy2011, tan2009r1)
    checkEquals(pRolocGUI:::.obsNewFoI(NULL, NULL, NULL, NULL), NULL)
    checkEquals(
        pRolocGUI:::.obsNewFoI(utList, NULL, NULL, NULL, "object1"), NULL)
    checkEquals(pRolocGUI:::.obsNewFoI(NULL, 1:10, NULL, NULL), NULL)
    checkEquals(pRolocGUI:::.obsNewFoI(NULL, NULL, "test", NULL), NULL)
    checkEquals(pRolocGUI:::.obsNewFoI(utList, NULL, NULL, 1, "object1"), NULL)
    checkEquals(pRolocGUI:::.obsNewFoI(NULL, 1:10, NULL, 1), NULL)
    checkEquals(pRolocGUI:::.obsNewFoI(NULL, NULL, "test", 1), NULL)
    checkEquals(pRolocGUI:::.obsNewFoI(utList, 1:10, NULL, 1, "object1"), NULL)
    checkEquals(
        pRolocGUI:::.obsNewFoI(utList, NULL, "test", 1, "object1"), NULL)
    checkEquals(foi(pRolocGUI:::.obsNewFoI(utList, 1:10, "test", 1, "object1")), 
        foi(FeaturesOfInterest(description = "test", 
        fnames = featureNames(andy2011)[1:10], object = andy2011)))
    checkEquals(foi(pRolocGUI:::.obsNewFoI(utList, 21:30, "test", 1, "object2")), 
        foi(FeaturesOfInterest(description = "test", 
        fnames = featureNames(tan2009r1)[21:30], object = tan2009r1)))
}
## END unit test .obsNewFoI ##



## START unit test for .fnamesFOI ## 
test_.fnamesFOI <- function() {
    ## taken from Examples of help page of Features of Interest
    x <- FeaturesOfInterest(
        description = "A traceable test set of features of interest",
        fnames = featureNames(tan2009r1)[1:10],
        object = tan2009r1)
    
    ## create FoICollection
    xx <- FoICollection()
    xx <- addFeaturesOfInterest(x, xx)
    
    ## features to compare with
    features <- c("P20353", "P53501", "Q7KU78", "P04412", "Q7KJ73", "Q7JZN0", 
                  "Q7KLV9", "Q9VM65", "Q9VCK0", "Q9VIU7")
    ## create list
    featuresList <- list(features)
    checkEquals(pRolocGUI:::.fnamesFOI(x), features)
    checkEquals(pRolocGUI:::.fnamesFOI(xx), featuresList)
}
## END unit test for .fnamesFOI ## 

## 
## no unit test for .showFOI
##

## START unit test for .dataSub ##
test_.dataSub <- function() {
    utList <- list(tan2009r1, tan2009r2)
    ## compute featureNames
    inter <- intersect(featureNames(tan2009r1), featureNames(tan2009r2))
    setdiffR1 <- setdiff(featureNames(tan2009r1), inter)
    setdiffR1 <- sort(setdiffR1)
    setdiffR2 <- setdiff(featureNames(tan2009r2), inter)
    res <- pRolocGUI:::.dataSub(utList)
    checkEquals(sort(featureNames(res[[1]])), sort(featureNames(tan2009r1)))
    checkEquals(sort(featureNames(res[[2]])), sort(featureNames(tan2009r2)))
    res <- pRolocGUI:::.dataSub(utList, "common")
    checkEquals(sort(featureNames(res[[1]])), sort(featureNames(tan2009r1[inter, ])))
    checkEquals(sort(featureNames(res[[2]])), sort(featureNames(tan2009r2[inter, ])))
    res <- pRolocGUI:::.dataSub(utList, "unique")
    checkEquals(sort(featureNames(res[[1]])), sort(featureNames(tan2009r1[setdiffR1, ])))
    checkEquals(sort(featureNames(res[[2]])), sort(featureNames(tan2009r2[setdiffR2, ])))
}
## END unit test for .dataSub ##

## start unit test for .mC ##
utList <- list(tan2009r1, tan2009r2)
markers <- c("unknown", "ER", "mitochondrion", "Golgi", "PM")

test_.mC <- function() {
    checkEquals(pRolocGUI:::.mC(utList, NULL, NULL), list())
    checkEquals(pRolocGUI:::.mC(utList, "markers", "markers"), 
        union(fData(tan2009r1)$markers, fData(tan2009r2)$markers))
    checkEquals(pRolocGUI:::.mC(utList, "markers", "PLSDA"), 
        union(fData(tan2009r1)$markers, fData(tan2009r2)$PLSDA))
}
## end unit test for .mC ##

## start unit test for .cellHTML ##
x <- 1:2
test_.cellHTML <- function() {
    checkEquals(pRolocGUI:::.cellHTML(NULL), "<td></td>")
    checkEquals(pRolocGUI:::.cellHTML("100"), "<td>100</td>")
    checkEquals(pRolocGUI:::.cellHTML(x), c("<td>1</td>", "<td>2</td>"))
}
## end unit test for .cellHTML ##

## start unit test for .boldHTML ##
x <- 1:2
test_.boldHTML <- function() {
    checkEquals(pRolocGUI:::.boldHTML(NULL), "<td><strong></strong></td>")
    checkEquals(pRolocGUI:::.boldHTML("100"), "<td><strong>100</strong></td>")
    checkEquals(pRolocGUI:::.boldHTML(x), 
        c("<td><strong>1</strong></td>", "<td><strong>2</strong></td>"))
}
## end unit test for .boldHTML

## start unit test for .rowHTML ##
mat <- matrix(1:9, 3, 3)
colnames(mat) <- c("common", "unique1", "unique2")
test_.rowHTML <- function() {
    checkEquals(pRolocGUI:::.rowHTML(mat[1,], "common", "1", "1"), 
        "<td><strong>1</strong></td><td>4</td><td>7</td></tr>")
    checkEquals(pRolocGUI:::.rowHTML(mat[1,], "common", "2", "1"), 
        "<td>1</td><td>4</td><td>7</td></tr>")
    checkEquals(pRolocGUI:::.rowHTML(mat[2,], "unique1", "2", "2"), 
        "<td>2</td><td><strong>5</strong></td><td>8</td></tr>")
    checkEquals(pRolocGUI:::.rowHTML(mat[3,], "unique2", "3", "3"), 
        "<td>3</td><td>6</td><td><strong>9</strong></td></tr>")
}
## end unit test for .rowHTML ##

##
## no unit test for .tableHTML
##

## START manual unit test for .plotPCA ## 

## select MSnSet andy2011 
## plot in tab "PCA" has to look like:
    ## initially
    valuesPCA <- plot2D(andy2011, fcol=NULL, dims = c(1, 2))

    ## select "x-axis" below "mirror 2nd object"
    plot2D(andy2011, fcol=NULL, mirrorX = TRUE)

    ## select in addition "y-axis" below "mirror 2nd object"
    plot2D(andy2011, fcol=NULL, mirrorX = TRUE, mirrorY = TRUE)
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
