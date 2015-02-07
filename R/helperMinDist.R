## Function to compute min2d distance from user input for PCA
.minDistPCA <- function(inputx, inputy, 
                            valuesx, valuesy) {
    if (!is.null(inputx) && !is.null(inputy)) {
        dist <- sqrt(
            (inputx - valuesx)^2 + ## x-component
                (inputy - valuesy)^2 ## y-component
        )
        ## compute the element (row index, i.e. the protein) which has the 
        ## shortest distance to the input (name will be returned)
        return(names(which(dist == min(dist))))
    }
}

## Function to compute min2d distance from user input for plotDist
## calculate protein nearest to user input
.minDistPlotDist <- function(obj, marker, org, inputx, inputy, 
                        ind = c("object1", "object2")) {
    if (!is.null(inputx) && !is.null(inputy)) {
        ind <- match.arg(ind)
        obj <- ifelse(ind == "object1", obj[1], obj[2])[[1]]
        
        if (inputx < (nrow(obj) + .3) && inputx > .5) {
        
            if (marker == "all")
                j <- 1:nrow(obj)
            else {
                j <- match(
                    subset(featureNames(obj), fData(obj)[, marker] == org),
                        featureNames(obj)
                )
            }
            dist <- abs(inputy - exprs(obj)[j, round(inputx, 0)])
            minDist <- min(dist)
            whichMinDist <- which(minDist == dist)
            ## return name
            return(names(whichMinDist))
        }
    }
}
