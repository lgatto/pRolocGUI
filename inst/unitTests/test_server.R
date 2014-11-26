## START manual unit test for pRolocVis ## 

## enter: pRolocVis(object=list(andy2011, tan2009r1, dunkley2006) 
## in console and press enter

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
       dims=c(1, 2))

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
       dims=c(1, 2))

highlightOnPlot(andy2011, 
                FeaturesOfInterest(description = "hoP",
                                   fnames = featureNames(andy2011)[indPlDist], 
                                   object = andy2011),
                args = list(
                    fcol = fvarLabels(andy2011)[1],
                    xlim = c(min(valuesPCA[, 1]), max(valuesPCA[, 1])),
                    ylim = c(min(valuesPCA[, 2]), max(valuesPCA[, 2])),
                    dims = c(1, 2), col="black", cex=1.5))

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
       dims=c(1, 2))

highlightOnPlot(andy2011, 
                FeaturesOfInterest(description = "hoP",
                                   fnames = featureNames(andy2011)[indQuery], 
                                   object = andy2011),
                args = list(
                    fcol = fvarLabels(andy2011)[1],
                    xlim = c(min(valuesPCA[, 1]), max(valuesPCA[, 1])),
                    ylim = c(min(valuesPCA[, 2]), max(valuesPCA[, 2])),
                    dims = c(1, 2), col="black", cex=1.5))

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
       dims=c(1, 2))

highlightOnPlot(andy2011, 
                FeaturesOfInterest(description = "hoP",
                                   fnames = featureNames(andy2011)[indSaSe], 
                                   object = andy2011),
                args = list(
                    fcol = fvarLabels(andy2011)[1],
                    xlim = c(min(valuesPCA[, 1]), max(valuesPCA[, 1])),
                    ylim = c(min(valuesPCA[, 2]), max(valuesPCA[, 2])),
                    dims = c(1, 2), col="black", cex=1.5))

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

## START manual unit test for pRolocComp ##  

## enter pRolocComp(object=list(tan2009r1, tan2009r2)) in console
par(mfrow=c(1, 2))
## START PCA click input##
valuesPCAr1 <- plot2D(tan2009r1, fcol = NULL, plot = FALSE)
valuesPCAr2 <- plot2D(tan2009r2, fcol = NULL, plot = FALSE)
## make sure that "object1" is selected
## select "markers" in "colour" 
## ("none" in symboltype, PC 1 (along x-axis) and 2 (along y-axis), 
## not zoomed)
## click in the left plot
## click on the feature belonging to "ER" at (2.47, -0.45)
indPCAr1 <- which(featureNames(tan2009r1) == "Q9VUZ0")
indPCAr2 <- which(featureNames(tan2009r2) == "Q9VUZ0")
## click on the feature belongin to "Golgi" at (2.76, 1.50)
indPCAr1 <- c(indPCAr1, which(featureNames(tan2009r1) == "Q9VR90")) 
indPCAr2 <- c(indPCAr2, which(featureNames(tan2009r2) == "Q9VR90")) 
## click on the feature belonging to "mitochondrion" at (-2.63, 0.55)
indPCAr1 <- c(indPCAr1, which(featureNames(tan2009r1) == "P91929"))
indPCAr2 <- c(indPCAr2, which(featureNames(tan2009r2) == "P91929"))
## change to "object2" and select "markers" in "colour"
## click in the right plot 
## click on the feature belonging to "unknown" at (-1.58 4.086)
indPCAr1 <- c(indPCAr1, which(featureNames(tan2009r1) == "O61604")) 
indPCAr2 <- c(indPCAr2, which(featureNames(tan2009r2) == "O61604")) 

## select "markers" in "colour" 
## ("none" in symboltype, PC 1 (along x-axis) and 2 (along y-axis), 
## not zoomed)
## the plot in tab "PCA" should look like this (make sure the 
## check box next to "PCA" is checked):
plot2D(tan2009r1, fcol="markers",
       xlim=c(min(valuesPCAr1[, 1]), max(valuesPCAr1[, 1])),
       ylim=c(min(valuesPCAr1[, 2]), max(valuesPCAr1[, 2])),
       dims=c(1, 2))

highlightOnPlot(tan2009r1, 
                FeaturesOfInterest(description = "hoP",
                                   fnames = featureNames(tan2009r1)[indPCAr1], 
                                   object = tan2009r1),
                args = list(
                    fcol = fvarLabels(tan2009r1)[1],
                    xlim = c(min(valuesPCAr1[, 1]), max(valuesPCAr1[, 1])),
                    ylim = c(min(valuesPCAr1[, 2]), max(valuesPCAr1[, 2])),
                    dims = c(1, 2), col="black", cex=1.5)
                )
plot2D(tan2009r2, fcol="markers",
       xlim=c(min(valuesPCAr2[, 1]), max(valuesPCAr2[, 1])),
       ylim=c(min(valuesPCAr2[, 2]), max(valuesPCAr2[, 2])),
       dims=c(1, 2)
       )
highlightOnPlot(tan2009r2, 
                FeaturesOfInterest(description = "hoP",
                                   fnames = featureNames(tan2009r2)[indPCAr2], 
                                   object = tan2009r2),
                args = list(
                    
                    fcol = fvarLabels(tan2009r2)[1],
                    xlim = c(min(valuesPCAr2[, 1]), max(valuesPCAr2[, 1])),
                    ylim = c(min(valuesPCAr2[, 2]), max(valuesPCAr2[, 2])),
                    dims = c(1, 2), col="black", cex=1.5))


## the plot in tab "protein profiles" should look like this (make sure the
## check box next to "PCA" is checked):
## select "1" in "number of plots to display"
## select "all" in "feature(s) in" and "all" in "assigned to"
plotDist(tan2009r1)
apply(X = exprs(tan2009r1[indPCAr1]), MARGIN = 1, FUN = lines) 
plotDist(tan2009r2)
apply(X = exprs(tan2009r2[indPCAr2]), MARGIN = 1, FUN = lines)
## the tab "quantitation" should contain these features (when radioButton
## "selected" is pressed), change between "object1" and "object2"
exprs(tan2009r1[indPCAr1])
exprs(tan2009r2[indPCAr2])

## the tab "feature meta-data" should contain these features (when 
## radioButton "selected" is pressed), 
## change between "object1" and "object2"
fData(tan2009r1[indPCAr1])
fData(tan2009r2[indPCAr2])
## END PCA click input ## 



## START plotDist click input ## 
## select "1" in "number of plots to display"
## select "all" in "feature(s) in" and "all" in "assigned to"
## click on the feature at (1, 0.61) in the left plot
indPlDistr1 <- which(featureNames(tan2009r1) == "O97428")
indPlDistr2 <- which(featureNames(tan2009r2) == "O97428")
## click on the feature at (1, 0.508) in the right plot 
indPlDistr1 <- c(indPlDistr1, which(featureNames(tan2009r1) == "Q7JR58"))
indPlDistr2 <- c(indPlDistr2, which(featureNames(tan2009r2) == "Q7JR58"))
## click on the feature at (4, 0.69) in the right plot
indPlDistr1 <- c(indPlDistr1, which(featureNames(tan2009r1) == "P84051"))
indPlDistr2 <- c(indPlDistr2, which(featureNames(tan2009r2) == "P84051"))

## the plot in tab "protein profiles" should look like this (make sure the 
## box next to "protein profiles" is selected:
plotDist(tan2009r1)
apply(X = exprs(tan2009r1[indPlDistr1]), MARGIN = 1, FUN = lines)
plotDist(tan2009r2)
apply(X = exprs(tan2009r2[indPlDistr2]), MARGIN = 1, FUN = lines)

## change to the tab "PCA"
## select "markers" in "colour" 
## ("none" in symboltype, PC 1 (along x-axis) and 2 (along y-axis), 
## not zoomed)
## the plot in tab "PCA" should look like this (make sure the check box
## next to "PCA" is checked):
plot2D(tan2009r1, fcol="markers",
       xlim=c(min(valuesPCAr1[, 1]), max(valuesPCAr1[, 1])),
       ylim=c(min(valuesPCAr1[, 2]), max(valuesPCAr1[, 2])),
       dims=c(1, 2))

highlightOnPlot(tan2009r1, 
                FeaturesOfInterest(description = "hoP",
                                   fnames = featureNames(tan2009r1)[indPlDistr1], 
                                   object = tan2009r1),
                args = list(
                    fcol = fvarLabels(tan2009r1)[1],
                    xlim = c(min(valuesPCAr1[, 1]), max(valuesPCAr1[, 1])),
                    ylim = c(min(valuesPCAr1[, 2]), max(valuesPCAr1[, 2])),
                    dims = c(1, 2), col="black", cex=1.5)
                )
plot2D(tan2009r2, fcol="markers",
       xlim=c(min(valuesPCAr2[, 1]), max(valuesPCAr2[, 1])),
       ylim=c(min(valuesPCAr2[, 2]), max(valuesPCAr2[, 2])),
       dims=c(1, 2)
       )
highlightOnPlot(tan2009r2, 
                FeaturesOfInterest(description = "hoP",
                                   fnames = featureNames(tan2009r2)[indPlDistr2], 
                                   object = tan2009r2),
                args = list(
                    fcol = fvarLabels(tan2009r2)[1],
                    xlim = c(min(valuesPCAr2[, 1]), max(valuesPCAr2[, 1])),
                    ylim = c(min(valuesPCAr2[, 2]), max(valuesPCAr2[, 2])),
                    dims = c(1, 2), col="black", cex=1.5))

## the tab "quantitation" should contain these features (when radioButton
## "selected" is pressed), change between "object1" and "object2"
exprs(tan2009r1[indPlDistr1])
exprs(tan2009r2[indPlDistr2])

## the tab "feature meta-data" should contain these features (when 
## radioButton "selected" is pressed), 
## change between "object1" and "object2"
fData(tan2009r1[indPlDistr1])
fData(tan2009r2[indPlDistr2])
## END plotDist click input ## 



## START query ##
## select "pd.markers" in "query" (Display selection widget)
## select "Lysosome" in the second drop-down list
## click on "Submit selection"
indQueryr1 <- which(fData(tan2009r1)$pd.markers == "Lysosome")
## select "pd.2013" in "query"
## select "Phenotype 5" in the second drop-down list
## click on "Submit selection"
indQueryr1 <- c(indQueryr1, which(fData(tan2009r1)$pd.2013 == "Phenotype 5"))
## select "protein" in "query" 
## select "AL3A2_HUMAN" in the second drop-down list
## click on "Submit selection"
indQueryr1 <- c(indQueryr1, which(featureNames(tan2009r1) == "O97428"))
indQueryr2 <- match(featureNames(tan2009r1)[indQueryr1], featureNames(tan2009r2))
indQueryr2 <- which(featureNames(tan2009r2)[indQueryr2] != "NA")
indQueryr1 <- unique(indQueryr1)
indQueryr2 <- unique(indQueryr2)
## select "markers" in "colour" 
## ("none" in symboltype, PC 1 (along x-axis) and 2 (along y-axis), 
## not zoomed)
## the plot in tab "PCA" should look like this (make sure the check box
## next to "query" is checked):
plot2D(tan2009r1, fcol="markers",
       xlim=c(min(valuesPCAr1[, 1]), max(valuesPCAr1[, 1])),
       ylim=c(min(valuesPCAr1[, 2]), max(valuesPCAr1[, 2])),
       dims=c(1, 2))

highlightOnPlot(tan2009r1, 
               FeaturesOfInterest(description = "hoP",
                                   fnames = featureNames(tan2009r1)[indQueryr1], 
                                   object = tan2009r1),
                args = list(
                    fcol = fvarLabels(tan2009r1)[1],
                    xlim = c(min(valuesPCAr1[, 1]), max(valuesPCAr1[, 1])),
                    ylim = c(min(valuesPCAr1[, 2]), max(valuesPCAr1[, 2])),
                    dims = c(1, 2), col="black", cex=1.5))

plot2D(tan2009r2, fcol="markers",
       xlim=c(min(valuesPCAr2[, 1]), max(valuesPCAr2[, 1])),
       ylim=c(min(valuesPCAr2[, 2]), max(valuesPCAr2[, 2])),
       dims=c(1, 2))

highlightOnPlot(tan2009r2, 
                FeaturesOfInterest(description = "hoP",
                                   fnames = featureNames(tan2009r2)[indQueryr2], 
                                   object = tan2009r2),
                args = list(
                    fcol = fvarLabels(tan2009r2)[1],
                    xlim = c(min(valuesPCAr2[, 1]), max(valuesPCAr2[, 1])),
                    ylim = c(min(valuesPCAr2[, 2]), max(valuesPCAr2[, 2])),
                    dims = c(1, 2), col="black", cex=1.5))


## the plot in tab "protein profiles" should look like this (make sure the
## check box next to "query" is checked):
## select "1" in "number of plots to display"
## select "all" in "feature(s) in" and "all" in "assigned to"
plotDist(tan2009r1)
apply(X = exprs(tan2009r1[indQueryr1]), MARGIN = 1, FUN = lines) 
plotDist(tan2009r2)
apply(X = exprs(tan2009r2[indQueryr2]), MARGIN = 1, FUN = lines)

## the tab "quantitation" should contain these features (when radioButton
## "selected" is pressed), change between "object1" and "object2"
exprs(tan2009r1[indQueryr1])
exprs(tan2009r2[indQueryr2])

## the tab "feature meta-data" should contain these features (when 
## radioButton "selected" is pressed), 
## change between "object1" and "object2"
fData(tan2009r1[indQueryr1])
fData(tan2009r2[indQueryr2])
## END query ##



## START saved searches ## 
## make sure "object1" is selected
## query for the features "Lysosome" in "pd.markers"
## (Display selection widget) and click on "Submit selection"
indSaSer1 <- which(fData(tan2009r1)$pd.markers == "Lysosome")
## query for "Phenotype 5" in "pd.2013" and click on "Submit selection"
indSaSer1 <- c(indSaSer1, which(fData(tan2009r1)$pd.2013 == "Phenotype 5"))
## query for "FK506-bp2" in "Flybase.Symbol" and click on "Submit selection"
indSaSer1 <- c(indSaSer1, which(featureNames(tan2009r1) == "P48375"))
indSaSer1 <- unique(indSaSer1)
indSaSer2 <- match(featureNames(tan2009r1)[indSaSer1], featureNames(tan2009r2))
indSaSer2 <- which(featureNames(tan2009r2)[indSaSer2] != "NA")
indSaSer2 <- unique(indQueryr2)


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
plot2D(tan2009r1, fcol="markers",
       xlim=c(min(valuesPCAr1[, 1]), max(valuesPCAr1[, 1])),
       ylim=c(min(valuesPCAr1[, 2]), max(valuesPCAr1[, 2])),
       dims=c(1, 2))

highlightOnPlot(tan2009r1, 
                FeaturesOfInterest(description = "hoP",
                                   fnames = featureNames(tan2009r1)[indSaSer1], 
                                   object = tan2009r1),
                args = list(
                    fcol = fvarLabels(tan2009r1)[1],
                    xlim = c(min(valuesPCAr1[, 1]), max(valuesPCAr1[, 1])),
                    ylim = c(min(valuesPCAr1[, 2]), max(valuesPCAr1[, 2])),
                    dims = c(1, 2), col="black", cex=1.5)
                )
plot2D(tan2009r2, fcol="markers",
       xlim=c(min(valuesPCAr2[, 1]), max(valuesPCAr2[, 1])),
       ylim=c(min(valuesPCAr2[, 2]), max(valuesPCAr2[, 2])),
       dims=c(1, 2)
       )
highlightOnPlot(tan2009r2, 
                FeaturesOfInterest(description = "hoP",
                                   fnames = featureNames(tan2009r2)[indSaSer2], 
                                   object = tan2009r2),
                args = list(
                    fcol = fvarLabels(tan2009r2)[1],
                    xlim = c(min(valuesPCAr2[, 1]), max(valuesPCAr2[, 1])),
                    ylim = c(min(valuesPCAr2[, 2]), max(valuesPCAr2[, 2])),
                    dims = c(1, 2), col="black", cex=1.5))

## the plot in tab "protein profiles" should look like this (make sure the
## check box next to "query" is checked):
## select "1" in "number of plots to display"
## select "all" in "feature(s) in" and "all" in "assigned to"
plotDist(tan2009r1)
apply(X = exprs(tan2009r1[indSaSer1]), MARGIN = 1, FUN = lines) 
plotDist(tan2009r2)
apply(X = exprs(tan2009r2[indSaSer2]), MARGIN = 1, FUN = lines)

## the tab "quantitation" should contain these features (when radioButton
## "selected" is pressed), change between "object1" and "object2"
exprs(tan2009r1[indSaSer1])
exprs(tan2009r2[indSaSer2])

## the tab "feature meta-data" should contain these features (when 
## radioButton "selected" is pressed)
fData(tan2009r1[indSaSer1])
fData(tan2009r2[indSaSer2])

## when quitting pRolocComp make sure that pRolocGUI_SearchResults was 
## assigned to .GlobalEnv (and unittest1 is the first element in the 
## FoICollection)
if (exists("pRolocGUI_SearchResults", .GlobalEnv)) {
    print("pRolocGUI_SearchResult exists in .GlobalEnv")
    if (identical(
        featureNames(tan2009r1)[indSaSer1], 
        foi(foi(pRolocGUI_SearchResults)[[1]]))
        ) {
        print("all features names are identical")
    } else 
        print("the feature names created are not identical")
}

## END saved searches ## 

