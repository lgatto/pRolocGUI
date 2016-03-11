.onAttach <- function(libname, pkgname) {
    msg <- paste0("\nThis is pRolocGUI version ", packageVersion("pRolocGUI"), "\n")
    if (packageVersion("DT") < "0.1.40")
        paste(msg, "\n", paste(redirectMsg(), collapse = ""))
    packageStartupMessage(msg)
}
