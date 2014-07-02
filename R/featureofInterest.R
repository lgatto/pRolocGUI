#'@title Test for Features of Interests or Collection of 
#'Features of Interest
#'
#'@name is.FeaturesOfInterest is.FoICollection
#'
#'@aliases is.FeaturesOfInterest 
#'
#'@aliases is.FoICollection
#' 
#'@description  \code{is.FeaturesOfInterest} tests if an object is of class 
#'\code{"FeaturesOfInterest"}, \code{is.FoIColection} if object is of class 
#'\code{"FoICollection"}, respectively. Returning \code{TRUE} or \code{FALSE}.
#'
#'@return \code{TRUE} or \code{FALSE}.
#' 
#'@author Thomas Naake <thomasnaake@@gmx.de>
#' 
#'@export
#'
#'@usage 
#'\code{is.FeaturesOfInterest(x)} 
#'\code{is.FoICollection(xx)}
#'
#'@param x An object tested of being of class \code{FeaturesOfInterest}
#'@param xx An object tested of being of class \code{FoICollection}
#' 
#'@examples \dontrun{
#' x <- FeaturesOfInterest(description = "A traceable test set 
#' of features of interest", fnames = featureNames(tan2009r1)[1:10], 
#' object = tan2009r1)
#' 
#' is.FeatureOfInterest(x)
#' 
#' y <- FeaturesOfInterest(description = "Non-traceable features of interest",
#'      fnames = featureNames(tan2009r1)[111:113])
#'      
#' xx <- FoICollection()
#' xx <- addFeaturesOfInterest(x, xx)
#' xx <- addFeaturesOfInterest(y, xx)
#' 
#' is.FoICollection(xx)
#' }
#' 
#'@seealso \code{\link[MSnbase]{FeaturesOfInterest}}
#' 
is.FeaturesOfInterest <- function(x){
  if(class(x) == "FeaturesOfInterest"){
    return(TRUE)}else{return(FALSE)}
}
#'@export
is.FoICollection <- function(xx){
  if(class(xx) == "FoICollection"){
    return(TRUE)}else{return(FALSE)}}

#'@title Return information about set of features
#' 
#'@name fnamesFOI descriptionFOI
#'
#'@aliases fnamesFOI descriptionFOI
#' 
#'@description \code{fnamesFOI} and \code{descriptionFOI} accept both 
#'\code{FeaturesOfInterest} and \code{FoICollection} objects and 
#'return \code{featureNames} and \code{description}.
#'
#'@param x An object of class \code{FeaturesOfInterest} or \code{FoICollection}
#'@param flist If \code{TRUE} fnamesFOI returns list-like object, if \code{FALSE} it will return a vector. Set \code{TRUE} or \code{FALSE}.
#'
#'@examples \dontrun{
#' x <- FeaturesOfInterest(description = "A traceable test set 
#' of features of interest", fnames = featureNames(tan2009r1)[1:10], 
#' object = tan2009r1)
#' 
#' fnamesFOI(x)
#' descriptionFOI(x)
#' 
#' y <- FeaturesOfInterest(description = "Non-traceable features of interest",
#'      fnames = featureNames(tan2009r1)[111:113])
#'      
#' xx <- FoICollection()
#' xx <- addFeaturesOfInterest(x, xx)
#' xx <- addFeaturesOfInterest(y, xx)
#' 
#' fnamesFOI(xx)
#' descriptionFOI(xx)
#' }
#' 
#' 
#'@seealso \code{\link[pRolocGUI]{is.FeaturesOfInterest}} and \code{\link[pRolocGUI]{is.FoICollection}}
#' 
#'@export 
fnamesFOI <- function(x, flist=TRUE){
  if(is.FeaturesOfInterest(x)){
    return(foi(x))
    }else if(is.FoICollection(x)){
      if (!isTRUE(flist)){
      y <- vector("character", length=0)
      for (i in 1:length(x)){
      y <- c(y, foi(foi(x)[[i]]))}}else{
      y <- list()
      for (i in 1:length(x)){
      y[[i]] <- foi(foi(x)[[i]])}} 
    return(y)}else{return(NULL)}}
#'@export
descriptionFOI <- function(x){
  if(is.FeaturesOfInterest(x)){
    return(description(x))
  }else if(is.FoICollection(x)){
    y <- vector("character", length=0)
    for (i in 1:length(x)){
      y <- c(y, description(foi(x)[[i]]))
    }
    return(y)}
  else{return(NULL)}}

#'@title .show.FOI
#'
#'@aliases show.FOI
#'
#'@description A function which gives a result similar to executing
#' \code{x} when \code{x} is of class {"FeaturesOfInterest"} with 
#' information about the number of identical feature names in the feature set
#' or a feature set in a collection of feature sets with regard to a specified 
#' \code{\link{"MSnSet"}}. It is a helper function used in pRolocGUI 
#' to display saved search results. 
#'
#'@usage .show.FOI(x, tan2009r1)
#'
#'@author Thomas Naake <thomasnaake@@gmx.de>
#'
#'@examples \dontrun{show.FOI(x, tan2009r1)}
#'
.show.FOI <- function(x, fMSnSet, index=1){
  if (is.FeaturesOfInterest(x)){
    .show.FOI <- capture.output(x)} else
  if (is.FoICollection(x)){
    .show.FOI <- capture.output(show(foi(x)[[index]]))
    }else{
      print("object not of class 'FeaturesOfInterest' or 'FoICollection'")
      break()}
  .show.FOI <- c(.show.FOI, "Therefrom in selected MSnSet:")
  if (is.FeaturesOfInterest(x)){
    .show.FOI <- c(.show.FOI, fnamesIn(x, fMSnSet, TRUE))}
  if (is.FoICollection(x)){
    .show.FOI <- c(.show.FOI, fnamesIn(foi(x)[[index]], fMSnSet, TRUE))}
  return(.show.FOI)
}


#'@title Helper functions for \code{reactivePoll} in \code{pRolocGUI}
#'
#'@name .digestFOI .readSR .descrFOI
#'
#'@aliases .digestFOI .readSR .descrFOI
#' 
#' @description \code{.digestFOI} is used as the \code{checkFunc} argument, \code{.readSR} and \colde{.descrFOI} as \code{valueFunc} in \code{reactivePoll}
#' 
.digestFOI <- function(){
  if (exists("pRolocGUI_SearchResults", envir = .GlobalEnv)){
    digest(get("pRolocGUI_SearchResults", .GlobalEnv))}else{
      return(NULL)
    }}

.readSR <- function(){
  if (exists("pRolocGUI_SearchResults", envir = .GlobalEnv)){
    get("pRolocGUI_SearchResults", .GlobalEnv)}else{
      return(NULL)
    }}

.descrFOI <- function(){
  if (exists("pRolocGUI_SearchResults", envir = .GlobalEnv)){
  substring(descriptionFOI(get("pRolocGUI_SearchResults", .GlobalEnv)), 1, 15)
  }else{return(NULL)}}






