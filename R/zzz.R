.onAttach <- function(libname, pkgname)
    packageStartupMessage(
        paste("\nThis is pRolocGUI version", packageVersion("pRoloc"), "\n\n",
              redirectMsg))
