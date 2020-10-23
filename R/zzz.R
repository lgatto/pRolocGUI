.onAttach <- function(libname, pkgname) {
    msg <- paste0("\nThis is pRolocGUI version ",
                  packageVersion("pRolocGUI"), "\n")
    packageStartupMessage(msg)
}