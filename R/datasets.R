#' Update the CRAN metadata dataset
#' 
#' Download metadata from all CRAN packages, and save it into a directory
#' (by default, \code{data}). This is used to create the \code{CRANmetadata}
#' dataset.
#' 
#' @param data_dir Output directory to write CRANmetadata.rda 
#' @param all Whether all versions should be downloaded, or just the latest version
#' (by default, only latest version)
#' @param ... Extra arguments passed on to \code{download_packages_metadata}
#' 
#' @return The CRANmetadata object, invisible
update_CRAN_metadata <- function(data_dir = "data", all = FALSE, ...) {
  dir.create(data_dir, showWarnings = FALSE)
  pkgs <- get_package_names()
  CRANmetadata <- download_packages_metadata(pkgs, all = all, ...)
  save(CRANmetadata, file = file.path(data_dir, "CRANmetadata.rda"))
  
  invisible(CRANmetadata)
}


#' Download CRAN metadata, including all versions of each package
#' 
#' Download all metadata from all CRAN packages, optionally saving it to a file.
#' 
#' @param outfile output .rdata file
#' @param ... extra arguments passed on to \code{download_packages_metadata},
#' such as \code{verbose}
#' 
#' @return The CRANmetadata object, invisible
download_all_CRAN_metadata <- function(outfile, ...) {
  pkgs <- get_package_names()
  CRANallmetadata <- download_packages_metadata(pkgs, all = TRUE, ...)
  save(CRANallmetadata, file = outfile)
  
  invisible(CRANmetadata)
}


#' Given a list with versions metadata from CRAN, create
#' the CRANversions dataset
#' 
#' @param all_metadata Named list of metadata for CRAN packages,
#' including all versions
#' @param data_dir directory to save the dataset to
#' 
#' @return The CRANversions dataset, invisibly
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' # this line may take 5-10 minutes
#' CRANallmetadata <- practice:::download_all_CRAN_metadata("all_metadata.rdata", verbose = TRUE)
#' 
#' update_CRAN_versions(CRANallmetadata)
#' 
#' }
update_CRAN_versions <- function(all_metadata, data_dir = "data") {
  CRANversions <- plyr::ldply(all_metadata,
              function(m) {
                data.frame(version = names(m$versions),
                           date = sapply(m$versions, function(v) v$date))
              },
              .id = "package")
  CRANversions <- dplyr::tbl_df(CRANversions)
  CRANversions$date <- lubridate::ymd_hms(CRANversions$date)
  
  save(CRANversions, file = file.path(data_dir, "CRANversions.rda"))
  
  invisible(CRANversions)
}



#' Update the CRAN practices dataset
#' 
#' Check all CRAN practices, and save them to a directory.
#' 
#' @param data_dir output directory to write CRANpractices.rda
#' @param src_dir directory of package sources
#' @param ... extra arguments passed on to \code{\link{check_practices}}
#' 
#' @return the CRANpractices dataset, invisibly
#' 
#' @details This works with the current version of the CRANmetadata dataset (make
#' sure you've re-built the package if you've updated that dataset).
update_CRAN_practices <- function(data_dir = "data", src_dir = NULL, ...) {
  dir.create(data_dir, showWarnings = FALSE)
  
  if (is.null(src_dir)) {
    pkgs <- get_package_names()
  } else {
    pkgs <- list.files(src_dir)
  }

  CRANpractices <- check_practices(pkgs, metadata_lst = CRANmetadata,
                                   src_dir = src_dir, ...)
  
  save(CRANpractices, file = file.path(data_dir, "CRANpractices.rda"))
  
  invisible(CRANpractices)
}


#' Metadata from all CRAN packages
#' 
#' A list of the metadata for each package.
#' 
#' @name CRANmetadata
#' 
#' @format A named list.
NULL


#' Checks on practices of each package in CRAN
#' 
#' A dataset with some practice checks on each package.
#' 
#' @name CRANpractices
#' 
#' @format A tbl_df with one row for each package.
NULL


#' Version release dates of each package in CRAN
#' 
#' The release date of each version of each package.
#' 
#' @name CRANversions
#' 
#' @format A tbl_df with one row for each version, and three columns:
#' \itemize{
#'   \item package. package name
#'   \item version. package version
#'   \item date. release date as a POSIXct object
#' }
NULL
