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
