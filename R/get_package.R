#'@title Retrieve a package source
#'@description \code{get_package_source} retrieves a package source, decompresses
#'it, and places it in a directory so that software engineering tests can be performed
#'on the contents. See the "See also" section for examples of these tests, and
#'\code{remove_package_source} to remove the source directory when you're done
#'using it.
#'
#'@param package_name the name of a package.
#'
#'@return a full, non-relative link to the directory the decompressed
#'package is stored in.
#'
#'@seealso
#'\code{\link{remove_package_source}} to remove the package,
#'\code{\link{get_package_metadata}} to get the metadata associated
#'with a package, or \code{\link{get_package_names}} to retrieve
#'a listing of the names of packages on CRAN.
#'
#'@examples
#'\dontrun{
#'#Get the package source for urltools
#'file_location <- get_package_source("urltools")
#'}
#'@export 
get_package_source <- function(package_name){
  
  local_temp <- tempdir()
  dl_link <- download.packages(package_name, destdir = local_temp, type = "source",
                               quiet = TRUE)[1,2]
  untar(file.path(dl_link), exdir = local_temp)
  file.remove(dl_link)
  
  return(file.path(local_temp,package_name))
}


#' Download multiple package sources from CRAN.
#' 
#' Download multiple package sources from CRAN, saving them to a
#' directory. This can be used in a meta-analysis of CRAN.
#' 
#' @param package_names character vector of one or more package names
#' @param dir directory to save packages to
#' @param quiet whether package download messages should be suppressed
#' 
#' @export
download_packages <- function(package_names, dir, quiet = TRUE) {
  dir.create(dir, showWarnings = FALSE)
  
  download0 <- dplyr::failwith(NULL, download.packages)
  
  for (package_name in package_names) {
    cat(package_name, "\n")
    if (file.exists(file.path(dir, package_name))) {
      next
    }
    dl <- download0(package_name, destdir = dir, type = "source", quiet = quiet)
    if (length(dl) != 0) {
      dl_link <- dl[1, 2]
      untar(file.path(dl_link), exdir = dir)
      file.remove(dl_link)
    }
  }
}


#'@title remove package source
#'@description removes the decompressed source code of a package,
#'retrieved with \code{get_package_source}, to free up storage
#'space and clean up after analysis.
#'
#'@param package_directory the full path to the directory, retrieved
#'with \code{\link{get_package_source}}.
#'
#'@export
remove_package_source <- function(package_directory){
  unlink(package_directory, recursive = TRUE, force = TRUE)
  return(invisible())
}


#'@title get the names of available packages
#'@description retrieves the names of available packages from whatever CRAN mirror
#'is associated with your session. These can then be passed into
#'\code{\link{get_package_metadata}} or \code{\link{get_package_source}}. The
#'results are cached locally after one call.
#'
#'@return a vector of package names that can be passed into \code{\link{get_package_metadata}}
#'or \code{\link{get_package_source}}.
#'
#'@seealso \code{\link{get_package_metadata}} or \code{\link{get_package_source}} for making
#'use of this information.
#'
#'@export
get_package_names <- function(){
  return(unname(available.packages())[,1])
}