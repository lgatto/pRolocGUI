narrowFeatureData <- function(object,
                              n1 = 6, n2 = 6,
                              fcol = "markers") {
    if (length(fcol) > 1)
        fcol <- NULL
    if (is.null(fcol)) {
        i <- selectFeatureData(object)
        fData(object) <- fData(object)[, i]
        return(object)
    }
    n <- n1 + n2
    fv <- fvarLabels(object)
    ln <- length(fv)
    if (ln <= n) return(object)
    i <- c(1:n1, (ln-n2+1):ln)
    if (any(fcol %in% fv) & !any(fcol %in% fv[i]))
        i <- c(1:n1, (ln-n2+2):ln, match(fcol, fv))
    fData(object) <- fData(object)[, i]
    if (validObject(object))
        return(object)
}



redirectMsg <- function() {
    msg <- c("The most recent version of pRolocGUI depends on DT version 0.1.40\n",
             "or higher, which is only available from GitHub. The official\n",
             "Bioconductor build infrastructure uses the package from CRAN,\n",
             "which is still at version 0.1. Hence, you are required to manually\n",
             "install a more recent version of DT.\n",
             "Please read the installation instructions on\n\n",
             "  https://github.com/ComputationalProteomicsUnit/pRolocGUI",
             "\n\nand/or run the function\n\n",
             "  pRolocGUI:::installDTfromGitHub().\n")
    paste(msg)
}

installDTfromGitHub <- function() {
    if (!require("devtools")) {
        message("Installing devtools from default CRAN mirror.")
        install.packages("devtools")
        require("devtools")
    }
    message("Installing DT from github.")
    devtools::install_github("rstudio/DT")
}
