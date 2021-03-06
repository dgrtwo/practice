% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/testing.R
\name{check_testing}
\alias{check_testing}
\title{identify a package's unit testing framework, if it has one}
\usage{
check_testing(package_directory)
}
\arguments{
\item{package_directory}{the full path to the directory, retrieved
with \code{\link{get_package_source}}.}
}
\value{
either "None" (no recogniseable tests), "RUnit" (RUnit-based
tests), "testthat" (testthat-based tests) or "Other" (some form
of hand-rolled testing).
}
\description{
takes a downloaded package and identifies if the package
has a unit testing framework, or anything indicative of automated testing.
}
\details{
identifying if a package has unit tests (and what form
those tests take) is a pain because the frameworks can be stored
in a lot of different ways.
RUnit can exist in a dedicated "tests" directory, or embedded in
the actual package's R code. testthat tends to standardise on a
"test" directory. And, of course, informal tests outside of these
frameworks can look like pretty much anything.

\code{check_testing} hunts through the .R files in a package
for calls to load RUnit; if some are found, the tests are RUnit-based.
The same test with "testthat" is used to determine if tests are
testthat-based - and if neither are found, the presence of a top-level
directory called "tests" leads to "Other".
}
\examples{
\dontrun{
#urltools uses testthat
file_location <- get_package_source("urltools")
check_testing(file_location)

#blme uses RUnit
file_location <- get_package_source("blme")
check_testing(file_location)

#fortunes has no tests.
file_location <- get_package_source("fortunes")
check_testing(file_location)
}
}
\seealso{
\code{\link{check_upstream_repository}}
to identify if a package has an upstream source anywhere
(GitHub, BitBucket) that users can use to provide patches and/or
bug reports, and the package index for more checks.
}

