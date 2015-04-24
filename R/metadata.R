# functions associated with package metadata

#'@title get metadata associated with a package on CRAN
#'@description pings the \href{http://crandb.r-pkg.org/}{crandb} CRAN metadata
#'service to retrieve metadata associated with a specific package.
#'
#' @param package_name the name of a package, which can be retrieved with
#' \code{\link{get_package_names}}
#' @param all whether to return all versions of the package, as opposed to
#' only the most recent
#'
#'@return a named list containing the metadata associated with the package.
#'
#'@examples
#'\dontrun{
#'#Get the metadata associated with dplyr
#'dplyr_metadata <- get_package_metadata("dplyr")
#'}
#'@seealso
#'\code{\link{get_package_source}} and \code{\link{remove_package_source}} for
#'the content of a package, and \code{\link{get_package_names}} to retrieve
#'a listing of the names of packages on CRAN.
#'
#'@importFrom httr GET content user_agent
#'@importFrom jsonlite fromJSON
#'@export
get_package_metadata <- function(package_name, all = TRUE){
  url <- paste0("http://crandb.r-pkg.org/", package_name)
  if (all) {
    url <- paste0(url, "/all")
  }
  results <- GET(url,
                 user_agent("practice - https://github.com/Ironholds/practice"))
  results <- content(results, as = "parsed")
  if (length(names(results)) == 2 && names(results) == c("error", "reason")) {
    stop(results$reason)
  }
  return(results)
}


#' Download metadata from multiple packages to a file
#' 
#' Download metadata from multiple packages to a file. This is useful for
#' larger analyses of CRAN.
#' 
#' @param package_names Character vector of package names
#' @param verbose Whether package names should be displayed as they are downloaded
#' @param ... Extra arguments passed on to \code{get_package_metadata}
#' 
#' @return A named list, one for each package, containing a list for each package
#' metadata. This list contains the contents from the CRAN API, with one
#' field added: \code{retrieved}, a \code{posixCT} object with the time that the
#' data was retrieved.
#' 
#' @export
download_packages_metadata <- function(package_names, verbose = FALSE, ...) {
  ret <- lapply(package_names, function(package_name) {
    if (verbose) {
      cat(package_name, sep = "\n")
    }
    ret <- get_package_metadata(package_name, ...)
    ret$retrieved <- Sys.time()
    ret
  })
  names(ret) <- package_names
  ret
}

