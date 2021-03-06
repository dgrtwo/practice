% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/testing.R
\name{check_versioning}
\alias{check_versioning}
\title{identify if a package is semantically versioned}
\usage{
check_versioning(package_metadata)
}
\arguments{
\item{package_metadata}{package metadata retrieved with \code{\link{get_package_metadata}},
or the name of a package (in which case \code{get_package_metadata} will be called
internally).}
}
\value{
TRUE if the package does follow the semantic versioning standard,
FALSE if not.
}
\description{
take a package's metadata and identify
from it whether the package follows the "semantic versioning"
convention, at least in theory. See details for, well,
details.
}
\details{
\href{http://semver.org/}{semantic versioning}
is a convention for identifying version numbers of a codebase
in a way that distinguishes major changes, minor changes
and patches by using three period-separated
groups of digits. As an example, 2.0.0 is semantically
versioned; 2.0 is not.

\code{check_versioning} takes the latest package version
number found in package_metadata and identifies whether
it follows this format (TRUE) or does not (FALSE).
}
\examples{
#Identify if ggplot2 is semantically versioned (it is)
ggplot_metadata <- get_package_metadata("ggplot2")
check_versioning(ggplot_metadata)

#Identify if fbRanks is semantically versioned (it isn't)
fbranks_metadata <- get_package_metadata("fbRanks")
check_versioning(fbranks_metadata)
}
\seealso{
\code{\link{check_vignettes}} to identify if a package has vignettes
and how they are built, \code{\link{check_roxygen}} to see if inline
documentation is built with roxygen2, and the package index for more
tests and checks.
}

