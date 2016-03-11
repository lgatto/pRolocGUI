.onAttach <- function(libname, pkgname)
    packageStartupMessage(
        paste0("\nThis is pRolocGUI version ", packageVersion("pRolocGUI"), "\n\n",
              paste(redirectMsg(), collapse = "")))
