## global settings and loading of external data

## increase and limit the upload of external data to 20MB
options(shiny.maxRequestSize=20*1024^2)

## load MSnSets from package pRolocdata
data("andy2011")
data("dunkley2006")
data("tan2009r1")

## set colour palette (semi-transparent colours)
#setStockcol(paste0(getStockcol(), 70))