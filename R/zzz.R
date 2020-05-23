.onAttach <- function(libname, pkgname) {
    msg <- paste0("\nThis is pRolocGUI version ",
                  packageVersion("pRolocGUI"), "\n")
    packageStartupMessage(msg)
}

  
## Update feature data and convert any columns that are matrices to
## vectors as otherwise in the shiny app these are displayed as a long
## vector of (1, 0, 0, 0, 0, 1, 0) etc.
.makeMatsVecs <- function(msnset) {
    .tn <- length(fvarLabels(msnset))
    chk <- vector(length = .tn)
    for (i in 1:.tn) {
        chk[i] <- is.matrix(fData(msnset)[, i])
    }
    if (any(chk)) {
        .ind <- which(chk)
        .nams <- fvarLabels(msnset)[.ind]
        .tmpnams <- paste0(.nams, format(Sys.time(), "%a%b%d%H%M%S%Y"))
        for (i in seq(.nams)) {
            msnset <- pRoloc::mrkMatToVec(msnset, mfcol = .nams[i],
                                          vfcol = .tmpnams[i])
        }
        fData(msnset)[, .nams] <- NULL
        fvarLabels(msnset)[match(.tmpnams, fvarLabels(msnset))] <- .nams
    }
    return(msnset)
}


## define DT columns
.defineDT <- function(labs, fcol) {
  if (length(labs) > 6) {
    .ind <- which(labs == fcol)
    .fvarL <- labs[-.ind]
    selDT <- c(.fvarL[1:5], fcol)
  } else {
    selDT <- labs[1:length(labs)]
  }
  return(selDT)
}


## check pmarkers, if not a matrix, convert to a matrix
## if fcol = NULL, convert to matrix
.chkMarkersMat <- function(pmarkers, object, fcol) {
  if (!inherits(pmarkers, "matrix")) {
    mName <- paste0("Markers", format(Sys.time(), "%a%b%d%H%M%S%Y"))
    if (fcol == "nullmarkers") {
      m <- matrix(1, ncol = 1, nrow = nrow(object))
      rownames(m) <- featureNames(object)
      colnames(m) <- fcol <- mName
      fData(object)[, mName] <- pmarkers <- m
      colnames(pmarkers) <- "unknown"
    } else {
      object <- mrkVecToMat(object, fcol, mfcol = mName)
      fcol <- mName
      pmarkers <- fData(object)[, fcol]
    }
  }
  return(list(pm = pmarkers, 
              obj = object, 
              mN = mName))
}

for (i in seq(object_coords)) {
  if (nrow(object_coords[[i]]) != nrow(object[[i]])) 
    stop(paste("Number of features in the matrix", i, ",and MSnSet differ."))
  if (!all.equal(rownames(object_coords[[i]]), featureNames(object[[i]]))) 
    warning(paste("Matrix", i, "rownames and feature names don't match"))
}

