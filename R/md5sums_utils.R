
#'  How to ensure quick loading of SomaScan data in tidyfr
#'
#'  These utility functions are supposed to aid versioning of the data. It should
#'  be called after the `data-export.R` step, after only 1 technical check.
#'  Then a user can generate and save md5sums and make the new data version
#'  available. These functions use the same input parameters as `data_module.R` 
#'
#' @param path \code{character}, the path where to find the folder specified in
#' the `name` parameter (eg: `/home/user/data/`)
#' @param name \code{character}, the folder containing version folders
#' @param version \code{character}, the version to look inside the `name` folder
#'
#' @return \code{character}, with md5sum hashes, while names are the absolute
#' paths to the file that md5sum has refers to
#' @export
#'
#' @examples
#' 
#' # The first part is to build a new data version, written as a pseudocode
#' # step 0 - you modified the data somehow, so redo technical checks on files
#' # step 1 - export a new version of somalogic data with export_tdf(...)
#'
#' # The following steps are my proposed additions
#'
#' # step 1 - use build_somalogic_data_Rds(...) to generate pre-processed and
#' #           quickly readable data.
#' # step 2 - save md5sum hashes in this new version's folder with 
#' #           also including the 2 data modules generated in step 1 
#' #          (save_versioned_md5sums(...))
#' # 
#' #################################################################
#' # now let's simulate what a user could do to load the data      #
#' #################################################################
#' path <- "/home/gantonello/chris_somascan_proteomics"
#' name <- "chris_somalogic_full"
#' version <- "1.0.3.2"
#' 
#' new_md5sums <- get_md5sums(path, name, version)
#' old_md5sums <- readRDS(file.path(path, name, version, paste0("md5sums_v_", version, ".Rds")))
#' 
#' if(!identical(new_md5sums, old_md5sums)){
#' stop(
#' paste("One or more files not match the expected md5sums hashes:",
#' print(
#' data.frame(
#'   filename = names(new_md5sums)[new_md5sums != old_md5sums],
#'   new_hash <- new_md5sums[new_md5sums != old_md5sums],
#'   old_hash <- old_md5sums[new_md5sums != old_md5sums]
#'   )
#'   )
#' }
#' 
#' # load soma_data
#' soma_data <- readRDS(file.path(path, name, version, "data", "soma_data.Rds"))
#'
#' # load soma_ann
#' soma_ann <- readRDS(file.path(path, name, version, "data", "soma_ann.Rds"))

build_somalogic_data_Rds <- function(path, name, version){
  soma <- data_module(name = name, 
                      version = version,
                      path = path)
  
  #' save somamers experimental data:
  #' rows: samples; columns: somamers and sample metadata
  saveRDS(
    data(soma), 
    file = file.path(path, name, version, "data", "soma_data.Rds")
  )
  
  #' save somamers metadata:
  #' rows = somamer, columns: somamers annotations and other metatadata
  saveRDS(
    labels(soma), 
    file = file.path(path, name, version, "data", "soma_ann.Rds")
  )
}

get_md5sums <- function(path, name, version){
  full_path <- file.path(path, name, version, "data")
  textfiles <- list.files(full_path, pattern = "*", full.names = T)
  md5sums <- sapply(textfiles, md5sum)
  return(md5sums)
}

save_versioned_md5sums <- function(path, name, version){
  md5sums <- get_md5sums(path, name, version)
  saveRDS(
    object = md5sums,
    file = file.path(path, name, version, paste0("md5sums_v_", version, ".Rds"))
    )
}

