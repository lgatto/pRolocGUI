\name{fnamesFOI descriptionFOI}
\alias{descriptionFOI}
\alias{fnamesFOI}
\alias{fnamesFOI descriptionFOI}
\title{Return information about set of features}
\usage{
fnamesFOI(x, flist = TRUE)
}
\arguments{
  \item{x}{An object of class \code{FeaturesOfInterest} or
  \code{FoICollection}}

  \item{flist}{If \code{TRUE} fnamesFOI returns list-like
  object, if \code{FALSE} it will return a vector. Set
  \code{TRUE} or \code{FALSE}.}
}
\description{
\code{fnamesFOI} and \code{descriptionFOI} accept both
\code{FeaturesOfInterest} and \code{FoICollection} objects
and return \code{featureNames} and \code{description}.
}
\examples{
\dontrun{
x <- FeaturesOfInterest(description = "A traceable test set
of features of interest", fnames = featureNames(tan2009r1)[1:10],
object = tan2009r1)

fnamesFOI(x)
descriptionFOI(x)

y <- FeaturesOfInterest(description = "Non-traceable features of interest",
     fnames = featureNames(tan2009r1)[111:113])

xx <- FoICollection()
xx <- addFeaturesOfInterest(x, xx)
xx <- addFeaturesOfInterest(y, xx)

fnamesFOI(xx)
descriptionFOI(xx)
}
}
\seealso{
\code{\link[pRolocGUI]{is.FeaturesOfInterest}} and
\code{\link[pRolocGUI]{is.FoICollection}}
}

