#' Functions to track, check and read CHRIS data sets/directories
#'
#' @noRd
NULL

#' Lists all available data modules from a CHRIS release.
#'
#' @param release `character(1)` defining the CHRIS data release.
#'
#' @param path `character(1)` defining the full path of the release directory
#'     containing the modules.
#'
#' @return `character` with the names of the CHRIS data sets (modules).
#'
#' @author Johannes Rainer
#'
#' @noRd
chrisDataModules <- function(release = chrisDataRelease(), path = character()) {
    if (!length(path))
        path <- file.path(chrisDataPath(), release)
    dirs <- list.dirs(path, full.names = TRUE, recursive = FALSE)
    ## check for each folder if it's a valid dataset and return it. Warning for other folders.
    is_ok <- vapply(dirs, .check_dataset_content,
                    stop = FALSE, FUN.VALUE = logical(1))
    failed <- dirs[!is_ok]
    if (length(failed))
        warning("The following folders do not represent ",
                "valid CHRIS data sets: ",
                paste0("'", basename(failed), "'", collapse = ", "))
    basename(dirs[is_ok])
    ## Maybe extract some info also from within the module? e.g. it's name?
}

#' @param x `character(1)` representing the path which contains the CHRIS
#'     data files.
#'
#' @param stop `logical(1)` to enable *silent mode*. If `FALSE` (the default)
#'     an error will cause a `stop` call. If `TRUE` a `logical(1)` is returned
#'     whether the data set is valid or not.
#'
#' @return `logical(1)` whether the data set is valid.
#'
#' @author Johannes Rainer
#'
#' @noRd
.check_dataset_content <- function(x, stop = TRUE) {
    fls <- dir(x)
    if (!all(basename(fls) %in% c("data.txt", "groups.txt", "grp_labels.txt",
                                  "info.txt", "labels.txt", "mapping.txt"))) {
        if (stop)
            stop("Folder ", x, " is missing one or more required data files.")
        else return(FALSE)
    }
    ## Test content of various data files.
    data <- .data(x)
    msgs <- character()
    msgs <- c(msgs, .valid_data(data, stop = stop))
    msgs <- c(msgs, .valid_info(.info(x), stop = stop))
    labels <- .labels(x)
    msgs <- c(msgs, .valid_labels(labels, stop = stop))
    ## Check that all columns in data are also present in labels.
    miss <- which(!(colnames(data) %in% c("aid", labels$label)))
    if (length(miss))
        msgs <- c(msgs, paste0("missing labels for data column(s) ",
                               paste0(colnames(data)[miss], collapse = ", ")))
    ## Check that we have mappings for all categorical variables.
    map <- .mapping(x)
    cats <- labels$label[labels$type == "categorical"]
    if (length(cats)) {
        miss <- which(!cats %in% map$label)
        if (length(miss))
            msgs <- c(msgs, "Missing encoding for categorical variables: ",
                      paste0(cats[miss], collapse = ", "))
    }
    if (stop && length(msgs))
        stop(msgs)
    !length(msgs)
}

#' @importFrom utils read.table
#'
#' @noRd
.read_dataset_file <- function(x, name) {
    suppressWarnings(
        read.table(paste0(x, "/", name, ".txt"), sep = "\t", header = TRUE))
}

.data <- function(x) {
    .read_dataset_file(x, "data")
}

.valid_data <- function(x, stop = TRUE) {
    msgs <- character()
    if (!any(colnames(x) == "aid"))
        msgs <- c(msgs, "data.txt lacks required column 'aid'")
    if (stop && length(msgs))
        stop(msgs)
    msgs
}

.groups <- function(x) {
    .read_dataset_file(x, "groups")
}

.valid_groups <- function(x, stop = TRUE) {
    msgs <- character()
    if (ncol(x) != 2)
        msgs <- c(msgs, "groups.txt is expected to have two columns.")
    if (!all(colnames(x) %in% c("group", "label")))
        msgs <- c(msgs, "groups.txt lacks one or more required columns.")
    if (stop && length(msgs))
        stop(msgs)
    msgs
}

.grp_labels <- function(x) {
    .read_dataset_file(x, "grp_labels")
}

.valid_grp_labels <- function(x, stop = TRUE) {
    msgs <- character()
    if (ncol(x) != 2)
        msgs <- c(msgs, "grp_labels.txt is expected to have two columns.")
    if (!all(colnames(x) %in% c("group", "description")))
        msgs <- c(msgs, "grp_labels.txt lacks one or more required columns.")
    if (stop && length(msgs))
        stop(msgs)
    msgs
}

.info <- function(x) {
    .read_dataset_file(x, "info")
}

.valid_info <- function(x, stop = TRUE) {
    msgs <- character()
    if (ncol(x) != 2)
        msgs <- c(msgs, "info.txt is expected to have two columns")
    if (!all(colnames(x) %in% c("key", "description")))
        msgs <- c(msgs,
                  paste0("columns of info.txt are expected to be called 'key' ",
                         "and 'description'"))
    else if (!all(c("name", "description", "version", "date") %in% x[, "key"]))
        msgs <- c(msgs, "info.txt is missing one or more required key entries")
    if (stop && length(msgs))
        stop(msgs)
    msgs
}

.labels <- function(x) {
    .read_dataset_file(x, "labels")
}

.valid_labels <- function(x, stop = TRUE) {
    msgs <- character()
    if (!all(c("label", "type", "min", "max", "missing", "description") %in%
             colnames(x)))
        msgs <- c(msgs, "labels.txt lacks one or more required columns.")
    else {
        if (any(is.na(x[, "label"])) | length(which(x[, "label"] == "")))
            msgs <- c(msgs, paste0("labels.txt: column 'label' contains",
                                   " missing or empty data"))
        if (any(!x[, "type"] %in% c("label", "character", "integer",
                                    "categorical", "float", "date", "time",
                                    "datetime", "boolean")))
            msgs <- c(
                msgs, "labels.txt: column 'type' contains unsupported values.")
    }
    if (stop && length(msgs))
        stop(msgs)
    msgs
}

.mapping <- function(x) {
    .read_dataset_file(x, "mapping")
}

.valid_mapping <- function(x, stop = TRUE) {
    msgs <- character()
    if (!all(c("label", "code", "value") %in% colnames(x)))
        msgs <- c(msgs, "mapping.txt misses one or more required columns")
    if (stop && length(msgs))
        stop(msgs)
    msgs
}

#' Check that we have for all categorical variables the correct encodings.
#'
#' @noRd
.valid_mapping_category_codes <- function(data, mapping, stop = TRUE) {
    msgs <- character()
    maps <- split(mapping$code, mapping$label)
    for (variable in names(maps)) {
        uvals <- unique(data[, variable])
        if (!all(uvals) %in% maps[[variable]])
            msgs <- c(msgs, paste0("Categorical variable '", variable,
                                   "' has values without encoding."))
    }
    if (stop && length(msgs))
        stop(msgs)
    msgs
}
