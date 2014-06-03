## START unit test for helperFunctionsServer.R ## 

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
