## START manual unit test for pRolocVis ## 

## select andy2011 in MSnSet

## START PCA click input##
    valuesPCA <- plot2D(andy2011, fcol = NULL, plot = FALSE)
    ## select "markers" in "colour" 
    ## ("none" in symboltype, PC 1 (along x-axis) and 2 (along y-axis), 
    ## not zoomed)
    ## click on the feature belonging to "ER" at (3.59, -1.54)
    indPCA <- which(featureNames(andy2011) == "PGAP1_HUMAN")
    ## click on the feature belongin to "PM" at (2.56, -2.24)
    indPCA <- c(indPCA, which(featureNames(andy2011) == "SLK_HUMAN")) 
    ## click on the feature belonging to "Golgi" at (-0.33, 1.92)
    indPCA <- c(indPCA, which(featureNames(andy2011) == "TPPC8_HUMAN"))
    ## click on the feature belonging to "Mitochondrion" at (-2.94, 3.99)
    indPCA <- c(indPCA, which(featureNames(andy2011) == "NDUS5_HUMAN")) 
    ## click on the feature belonging to "unknown" at (-4.64, 5.43)
    indPCA <- c(indPCA, which(featureNames(andy2011) == "HMGB1_HUMAN"))
    ## select "markers" in "colour" 
    ## ("none" in symboltype, PC 1 (along x-axis) and 2 (along y-axis), 
    ## not zoomed)
    ## the plot in tab "PCA" should look like this (make sure the 
    ## check box next to "PCA" is checked):
    plot2D(andy2011, fcol="markers",
        xlim=c(min(valuesPCA[, 1]), max(valuesPCA[, 1])),
        ylim=c(min(valuesPCA[, 2]), max(valuesPCA[, 2])),
        dims=c(1, 2)
    )
    highlightOnPlot(andy2011, 
        FeaturesOfInterest(description = "hoP",
            fnames = featureNames(andy2011)[indPCA], 
            object = andy2011),
        args = list(
            fcol = fvarLabels(andy2011)[1],
            xlim = c(min(valuesPCA[, 1]), max(valuesPCA[, 1])),
            ylim = c(min(valuesPCA[, 2]), max(valuesPCA[, 2])),
            dims = c(1, 2), col="black", cex=1.5)
    )
    
    ## the plot in tab "protein profiles" should look like this (make sure the
    ## check box next to "PCA" is checked):
    ## select "1" in "number of plots to display"
    ## select "all" in "feature(s) in" and "all" in "assigned to"
    plotDist(andy2011)
    apply(X = exprs(andy2011[indPCA]), MARGIN = 1, FUN = lines) 
    
    ## the tab "quantitation" should contain these features (when radioButton
    ## "selected" is pressed)
    exprs(andy2011[indPCA])
    
    ## the tab "feature meta-data" should contain these features (when 
    ## radioButton "selected" is pressed)
    fData(andy2011[indPCA])
## END PCA click input ## 



## START plotDist click input ## 
    ## select "1" in "number of plots to display"
    ## select "all" in "feature(s) in" and "all" in "assigned to"
    ## click on the feature at (1, 0.65)
    indPlDist <- which(featureNames(andy2011) == "BCD1_HUMAN")
    ## click on the feature at (2, 0.47)
    indPlDist <- c(indPlDist, which(featureNames(andy2011) == "PGAP1_HUMAN"))
    ## click on the feature at (4, 0.34)
    indPlDist <- c(indPlDist, which(featureNames(andy2011) == "L12R1_HUMAN"))
    ## click on the feature at (5, 0.44)
    indPlDist <- c(indPlDist, which(featureNames(andy2011) == "SPTA2_HUMAN"))
    ## click on the feature at (6, 0.60)
    indPlDist <- c(indPlDist, which(featureNames(andy2011) == "DHE3_HUMAN"))
    ## click on the feature at (7, 0.47)
    indPlDist <- c(indPlDist, which(featureNames(andy2011) == "PARP1_HUMAN"))
    ## click on the feature at (8, 0.91)
    indPlDist <- c(indPlDist, which(featureNames(andy2011) == "MCM6_HUMAN"))
    ## the plot in tab "protein profiles" should look like this (make sure the 
    ## box next to "protein profiles" is selected:
    plotDist(andy2011)
    apply(X = exprs(andy2011[indPlDist]), MARGIN = 1, FUN = lines)
    
    ## change to the tab "PCA"
    ## select "markers" in "colour" 
    ## ("none" in symboltype, PC 1 (along x-axis) and 2 (along y-axis), 
    ## not zoomed)
    ## the plot in tab "PCA" should look like this (make sure the check box
    ## next to "PCA" is checked):
    plot2D(andy2011, fcol="markers",
           xlim=c(min(valuesPCA[, 1]), max(valuesPCA[, 1])),
           ylim=c(min(valuesPCA[, 2]), max(valuesPCA[, 2])),
           dims=c(1, 2)
    )
    highlightOnPlot(andy2011, 
        FeaturesOfInterest(description = "hoP",
            fnames = featureNames(andy2011)[indPlDist], 
            object = andy2011),
        args = list(
            fcol = fvarLabels(andy2011)[1],
            xlim = c(min(valuesPCA[, 1]), max(valuesPCA[, 1])),
            ylim = c(min(valuesPCA[, 2]), max(valuesPCA[, 2])),
            dims = c(1, 2), col="black", cex=1.5)
    )
    
    ## the tab "quantitation" should contain these features (when radioButton
    ## "selected" is pressed)
    exprs(andy2011[indPlDist])
    
    ## the tab "feature meta-data" should contain these features (when 
    ## radioButton "selected" is pressed)
    fData(andy2011[indPlDist])
## END plotDist click input ## 

    
    
## START query ##
    ## select "pd.markers" in "query" (Display selection widget)
    ## select "Lysosome" in the second drop-down list
    ## click on "Submit selection"
    indQuery <- which(fData(andy2011)$pd.markers == "Lysosome")
    ## select "pd.2013" in "query"
    ## select "Phenotype 5" in the second drop-down list
    ## click on "Submit selection"
    indQuery <- c(indQuery, which(fData(andy2011)$pd.2013 == "Phenotype 5"))
    ## select "protein" in "query" 
    ## select "AL3A2_HUMAN" in the second drop-down list
    ## click on "Submit selection"
    indQuery <- c(indQuery, which(featureNames(andy2011) == "AL3A2_HUMAN"))
    
    ## select "markers" in "colour" 
    ## ("none" in symboltype, PC 1 (along x-axis) and 2 (along y-axis), 
    ## not zoomed)
    ## the plot in tab "PCA" should look like this (make sure the check box
    ## next to "query" is checked):
    plot2D(andy2011, fcol="markers",
        xlim=c(min(valuesPCA[, 1]), max(valuesPCA[, 1])),
        ylim=c(min(valuesPCA[, 2]), max(valuesPCA[, 2])),
        dims=c(1, 2)
    )
    highlightOnPlot(andy2011, 
        FeaturesOfInterest(description = "hoP",
            fnames = featureNames(andy2011)[indQuery], 
            object = andy2011),
        args = list(
            fcol = fvarLabels(andy2011)[1],
            xlim = c(min(valuesPCA[, 1]), max(valuesPCA[, 1])),
            ylim = c(min(valuesPCA[, 2]), max(valuesPCA[, 2])),
            dims = c(1, 2), col="black", cex=1.5)
    )
    
    ## the plot in tab "protein profiles" should look like this (make sure the
    ## check box next to "query" is checked):
    ## select "1" in "number of plots to display"
    ## select "all" in "feature(s) in" and "all" in "assigned to"
    plotDist(andy2011)
    apply(X = exprs(andy2011[indQuery]), MARGIN = 1, FUN = lines) 
    
    ## the tab "quantitation" should contain these features (when radioButton
    ## "selected" is pressed)
    exprs(andy2011[indQuery])
    
    ## the tab "feature meta-data" should contain these features (when 
    ## radioButton "selected" is pressed)
    fData(andy2011[indQuery])
## END query ##
    
    
    
## START saved searches ## 
    ## query for the features "Lysosome" in "pd.markers"
    ## (Display selection widget) and click on "Submit selection"
    indSaSe <- which(fData(andy2011)$pd.markers == "Lysosome")
    ## query for "Phenotype 5" in "pd.2013" and click on "Submit selection"
    indSaSe <- c(indSaSe, which(fData(andy2011)$pd.2013 == "Phenotype 5"))
    ## query for "AL3A2_HUMAN" in "protein" and click on "Submit selection"
    indSaSe <- c(indSaSe, which(featureNames(andy2011) == "AL3A2_HUMAN"))
    indSaSe <- unique(indSaSe)
    ## make sure the checkBox next to query is checked and go to the tab
    ## "searches" 
    ## enter an appropriate name (e.g. unittest1) in the text field and 
    ## click on "Create new features of interest"
    ## change to the tab "PCA"
    ## select "markers" in "colour" 
    ## ("none" in symboltype, PC 1 (along x-axis) and 2 (along y-axis), 
    ## not zoomed)
    ## click on "Clear features" or uncheck the box next to "query"
    ## check the box next to "saved searches" 
    ## the plot in tab "PCA" should look like this:
    plot2D(andy2011, fcol="markers",
        xlim=c(min(valuesPCA[, 1]), max(valuesPCA[, 1])),
        ylim=c(min(valuesPCA[, 2]), max(valuesPCA[, 2])),
        dims=c(1, 2)
    )
    highlightOnPlot(andy2011, 
        FeaturesOfInterest(description = "hoP",
            fnames = featureNames(andy2011)[indSaSe], 
            object = andy2011),
        args = list(
            fcol = fvarLabels(andy2011)[1],
            xlim = c(min(valuesPCA[, 1]), max(valuesPCA[, 1])),
            ylim = c(min(valuesPCA[, 2]), max(valuesPCA[, 2])),
            dims = c(1, 2), col="black", cex=1.5)
    )
    
    ## the plot in tab "protein profiles" should look like this (make sure the
    ## check box next to "query" is checked):
    ## select "1" in "number of plots to display"
    ## select "all" in "feature(s) in" and "all" in "assigned to"
    plotDist(andy2011)
    apply(X = exprs(andy2011[indSaSe]), MARGIN = 1, FUN = lines) 
    
    ## the tab "quantitation" should contain these features (when radioButton
    ## "selected" is pressed)
    exprs(andy2011[indSaSe])
    
    ## the tab "feature meta-data" should contain these features (when 
    ## radioButton "selected" is pressed)
    fData(andy2011[indSaSe])
    
    ## when quitting pRolocVis make sure that pRolocGUI_SearchResults was 
    ## assigned to .GlobalEnv (and unittest1 is the first element in the 
    ## FoICollection)
    if (exists("pRolocGUI_SearchResults", .GlobalEnv)) {
        print("pRolocGUI_SearchResult exists in .GlobalEnv")
        if (identical(
            featureNames(andy2011)[indSaSe], 
            foi(foi(pRolocGUI_SearchResults)[[1]]))
        ) {
            print("all features names are identical")
        } else 
            print("the feature names created are not identical")
    }
        
## END saved searches ## 
    
    
    
## END manual unit test for pRolocVis ## 