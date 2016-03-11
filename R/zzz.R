.onAttach <- function(libname, pkgname)
    packageStartupMessage(
        paste("\nThis is pRolocGUI version", packageVersion("pRolocGUI"), "\n"))
