% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/links.R
\name{links_to}
\alias{links_to}
\title{identify packages that a package mentions}
\usage{
links_to(package_metadata, with_versions = FALSE)
}
\arguments{
\item{package_metadata}{package metadata retrieved with \code{\link{get_package_metadata}},
or the name of a package (in which case \code{get_package_metadata} will be called
internally).}

\item{with_versions}{whether to return the version number (or lack thereof) as well as the
mentioned package's name. Set to FALSE by default.}
}
\value{
a vector of package names, if \code{with_versions} is FALSE, or a data.frame
of "package" and "version", if TRUE.
}
\description{
this identifies unique packages that are linked to by the package
that is being evaluated, in the context of dependencies, suggests and LinkingTo fields.
}

