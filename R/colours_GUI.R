#'@name colours_GUI
#'
#'@aliases colours_GUI
#'
#'@title Colours for pRolocGUI
#'
#'@description Sets the default colours as used in the package \code{pRoloc}.
#'
#'@usage colours_GUI(transparency=70)
#'
#'@export colours_GUI
#'
#'@param transparency Sets the transparency of plotted elements. 
#'Between 0 and 100 and of type \code{numeric}. Default is 
#'70 as used in \code{\link{pRoloc::pRoloc-Tutorial}}. 
#'
#'@author Thomas Naake <thomasnaake@@gmx.de>
#'
#'@examples \dontrun{colours_GUI(transparency=70)}
#'
#'@seealso \code{\link[pRoloc]{setStockcol}}
#'
colours_GUI <- function(transparency=70){
  if (is.numeric(transparency) &&
      transparency >= 0 && transparency <= 100){
  if (nchar(getStockcol()[1])==7){
  setStockcol(paste0(getStockcol(), transparency))}}}
