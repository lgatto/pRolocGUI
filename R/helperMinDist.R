## Function to compute min2d distance from user input for PCA
.minDistPCA <- function(inputx, inputy, valuesx, valuesy) {
    dist <- sqrt(
        (inputx - valuesx)^2 + ## x-component
            (inputy - valuesy)^2 ## y-component
    )
    minDist2d <- min(dist)
    ## compute the element (row index, i.e. the protein) which has the 
    ## shortest distance to the input (index will be returned)
    return(which(dist == minDist2d))
}

## Function to compute min2d distance from user input for plotDist
## calculate protein nearest to user input
.minDistPlotDist <- function(data, marker, org, inputx, inputy) {
    if (marker == "all")
        j <- 1:nrow(data[[1]])
    else {
        j <- match(
            subset(featureNames(data[[1]]), fData(data)[, marker] == org),
                featureNames(data[[1]])
        )
    }
    dist <- abs(inputy - exprs(data[[1]])[j, round(inputx, 0)])
    minDist <- min(dist)
    whichMinDist <- which(minDist == dist)
    ## return index
    return(j[whichMinDist[[1]]])
}
