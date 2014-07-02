\name{is.FeaturesOfInterest is.FoICollection}
\alias{is.FeaturesOfInterest}
\alias{is.FeaturesOfInterest is.FoICollection}
\title{Test for Features of Interests or Collection of
Features of Interest}
\usage{
\code{is.FeaturesOfInterest(x)}
\code{is.FoICollection(xx)}
}
\arguments{
  \item{x}{An object tested of being of class
  \code{FeaturesOfInterest}}

  \item{xx}{An object tested of being of class
  \code{FoICollection}}
}
\value{
\code{TRUE} or \code{FALSE}.
}
\description{
\code{is.FeaturesOfInterest} tests if an object is of class
\code{"FeaturesOfInterest"}, \code{is.FoIColection} if
object is of class \code{"FoICollection"}, respectively.
Returning \code{TRUE} or \code{FALSE}.
}
\examples{
\dontrun{
x <- FeaturesOfInterest(description = "A traceable test set
of features of interest", fnames = featureNames(tan2009r1)[1:10],
object = tan2009r1)

is.FeatureOfInterest(x)

y <- FeaturesOfInterest(description = "Non-traceable features of interest",
     fnames = featureNames(tan2009r1)[111:113])

xx <- FoICollection()
xx <- addFeaturesOfInterest(x, xx)
xx <- addFeaturesOfInterest(y, xx)

is.FoICollection(xx)
}
}
\author{
Thomas Naake <thomasnaake@gmx.de>
}
\seealso{
\code{\link[MSnbase]{FeaturesOfInterest}}
}

