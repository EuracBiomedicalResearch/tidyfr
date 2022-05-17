#' Functionality to deal with CHRIS releases, base CHRIS data path etc.
#'
#' CHRIS data is organized in the following way:
#'
#' CHRIS base path/module/version/data
#'                                  doc
#'
#' @noRd
NULL

#' @title CHRIS data path
#'
#' @name chris_data_path
#'
#' @description
#'
#' CHRIS data is organized in a hierarchical folder structure. The functions
#' documented here allow to define the base CHRIS data path and to list
#' available data modules.
#'
#' The available functions are:
#'
#' - `chris_data_path`: returns a `character(1)` with the currently defined full
#'   path to the CHRIS data.
#'
#' - `list_data_modules`: lists available data modules in the specified path.
#'
#' @param path `character(1)` specifying the directory where to look for CHRIS
#'     data modules.
#'
#' @return See the individual function descriptions.
#'
#' @author Johannes Rainer
#'
#' @md
#'
NULL

#' @rdname chris_data_path
#'
#' @export
chris_data_path <- function() {
    path <- Sys.getenv("CHRIS_DATA_PATH")
    if (path == "") {
        warning("CHRIS data path not set. The path can be globally set with ",
                "the environment variable 'CHRIS_DATA_PATH'")
    }
    path
}

#' @rdname chris_data_path
#'
#' @export
list_data_modules <- function(path = chris_data_path()) {
    if (!dir.exists(path))
        stop("Directory \"", path, "\" does not exist")
    mods <- list.dirs(path, full.names = TRUE, recursive = FALSE)
    res <- do.call(rbind, lapply(mods, function(z) {
        vers <- list.dirs(z, full.names = TRUE, recursive = FALSE)
        vers <- vers[length(vers) > 0]
        do.call(rbind, lapply(vers, function(y, module) {
            mod_path <- file.path(y, "data")
            if (dir.exists(mod_path) &&
                is.logical(.valid_data_directory(mod_path))) {
                inf <- .info(mod_path)
                data.frame(
                    name = module, version = basename(y),
                    description = inf$description[inf$key == "description"])
            } else data.frame()
        }, module = basename(z)))
    }))
    if (!length(res))
        warning("No CHRIS data modules found in \"", path, "\n")
    res
}
