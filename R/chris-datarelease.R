#' Functionality to deal with CHRIS releases, base CHRIS data path etc.
#'
#' CHRIS data is organized in the following way:
#'
#' <CHRIS base path>/module/version/data
#'                                  doc
#'
#' @noRd
NULL

#' @title CHRIS data path and data releases
#'
#' @name chrisDataPath
#'
#' @description
#'
#' CHRIS data is organized in a hierarchical folder structure. The functions
#' documented here allow to define the base CHRIS data path or to list and
#' select available data releases.
#'
#' The available functions are:
#'
#' - `chrisDataPath`: returns a `character(1)` with the currently defined full
#'   path to the CHRIS data.
#'
#' - `setChrisDataPath`: sets/changes the path to the CHRIS data. The provided
#'   `character(1)` needs to be the full path to the folder containing the
#'   CHRIS data. Before setting the path, the function first evaluates if it
#'   contains at least one valid data release folder.
#'
#' - `chrisDataReleases`: lists the data releases (folders) available in the
#'   CHRIS data path.
#'
#' - `setChrisDataRelease`: defines the CHRIS release that should be used in the
#'   current R session. Parameter `release` should be the name of the release
#'   (which corresponds to the name of the folder in which the data modules for
#'   the release are located).
#'
#' - `chrisDataRelease`: returns the currently set CHRIS release.
#'
#' @param path `character(1)` defining the path where the CHRIS data can be
#'     found.
#'
#' @param release `character(1)` with the name of the CHRIS data release.
#'
#' @return See the individual function descriptions.
#'
#' @author Johannes Rainer
#'
#' @md
#'
NULL

#' @rdname chrisDataPath
#'
#' @export
chrisDataPath <- function() {
    path <- Sys.getenv("CHRIS_DATA_PATH")
    if (path == "") {
        message("CHRIS data path not set. Either use 'setChrisDataPath' to set",
                " the path or set it useing environment variable ",
                "'CHRIS_DATA_PATH'")
    }
    path
}

#' Check the provided path for directories that contain at least one valid
#' release.
#'
#' @param path `character(1)` with the path to the CHRIS data.
#'
#' @return logical(1) whether the path contains at least one release.
#'
#' @author Johannes Rainer
#'
#' @noRd
.valid_chris_data_path <- function(path = character()) {
    dirs <- list.dirs(path, full.names = TRUE, recursive = FALSE)
    is_ok <- vapply(dirs, .valid_chris_release, logical(1))
    any(is_ok)
}

#' Check if the provided folder contains at least one valid data set.
#'
#' @param path `character(1)` with the path of the release.
#'
#' @return logical(1) whether the release contains at least one valid data set.
#'
#' @author Johannes Rainer
#'
#' @noRd
.valid_chris_release <- function(path = character()) {
    dirs <- list.dirs(path, full.names = TRUE, recursive = FALSE)
    is_ok <- vapply(dirs, .check_dataset_content, stop = FALSE,
                    FUN.VALUE = logical(1))
    any(is_ok)
}

#' @rdname chrisDataPath
#'
#' @export
setChrisDataPath <- function(path = character()) {
    if (!.valid_chris_data_path(path))
        stop("Provided path does not contain any CHRIS resources.")
    Sys.setenv(CHRIS_DATA_PATH = path)
}

#' @rdname chrisDataPath
#'
#' @export
chrisDataReleases <- function(path = chrisDataPath()) {
    dirs <- list.dirs(path, full.names = TRUE, recursive = FALSE)
    is_ok <- vapply(dirs, .valid_chris_release, logical(1))
    failed <- dirs[!is_ok]
    if (length(failed))
        warning("The following folders dont represent valid CHRIS ",
                "data release: ",
                paste0("'", basename(failed), "'", collapse = ", "))
    basename(dirs[is_ok])
}

#' @rdname chrisDataPath
#'
#' @export
chrisDataRelease <- function() {
    rel <- Sys.getenv("CHRIS_DATA_RELEASE")
    if (rel == "") {
        message("CHRIS data release not set. Either use 'setChrisDataRelease' ",
                "or define an environment variable 'CHRIS_DATA_RELEASE'")
    }
    rel
}

#' @rdname chrisDataPath
#'
#' @export
setChrisDataRelease <- function(release = chrisDataRelease()) {
    if (!.valid_chris_release(file.path(chrisDataPath(), release)))
        stop("'", release, "' is not a valid CHRIS release.")
    Sys.setenv(CHRIS_DATA_RELEASE = release)
}
