#' Functions to track, check and read CHRIS data sets/directories
#'
#' @noRd

#' @param x `character(1)` representing the path which contains the CHRIS
#'     data files.
#'
#' @param silent `logical(1)` to enable *silent mode*. If `FALSE` (the default)
#'     an error will cause a `stop` call. If `TRUE` a `logical(1)` is returned
#'     whether the data set is valid or not.
#' 
#' @return `logical(1)` whether the data set is valid.
#' 
#' @author Johannes Rainer
#'
#' @noRd
.check_dataset_content <- function(x, silent = FALSE) {
    fls <- dir(x)
    if (!all(basename(fls) %in% c("data.txt", "groups.txt", "grp_labels.txt",
                                  "info.txt", "labels.txt", "mapping.txt"))) {
        if (silent) return(FALSE)
        stop("Folder ", x, " is missing one or more required data files.")
    }
    ## Test content of various data files.
    data <- .data(x)
    .valid_data(data)
    .valid_info(.info(x))
    labels <- .labels(x)
    .valid_labels(labels)
    ## Check that all columns in data are also present in labels.
    miss <- which(!(colnames(data) %in% c("aid", labels$label)))
    if (length(miss))
        stop("missing labels for data column(s) ",
             paste0(colnames(data)[miss], collapse = ", "))
    ## Check that we have mappings for all categorical variables.
    map <- .mapping(x)
    cats <- labels$label[labels$type == "categorical"]
    if (length(cats)) {
        miss <- which(!cats %in% map$label)
        if (length(miss))
            stop("Missing encoding for categorical variables: ",
                 paste0(cats[miss], collapse = ", "))
    }
    invisible(TRUE)
}

.read_dataset_file <- function(x, name) {
    suppressWarnings(
        read.table(paste0(x, "/", name, ".txt"), sep = "\t", header = TRUE))
}

.data <- function(x) {
    .read_dataset_file(x, "data")
}

.valid_data <- function(x) {
    if (!any(colnames(x) == "aid"))
        stop("data.txt lacks required column 'aid'")
    invisible(TRUE)
}

.groups <- function(x) {
    .read_dataset_file(x, "groups")
}

.valid_groups <- function(x) {
    if (ncol(x) != 2)
        stop("groups.txt is expected to have two columns.")
    if (!all(colnames(x) == c("group", "label")))
        stop("groups.txt lacks one or more required columns.")
    invisible(TRUE)
}

.grp_labels <- function(x) {
    .read_dataset_file(x, "grp_labels")
}

.valid_grp_labels <- function(x) {
    if (ncol(x) != 2)
        stop("grp_labels.txt is expected to have two columns.")
    if (!all(colnames(x) == c("group", "description")))
        stop("grp_labels.txt lacks one or more required columns.")
    invisible(TRUE)
}

.info <- function(x) {
    .read_dataset_file(x, "info")
}

.valid_info <- function(x) {
    if (ncol(x) != 2)
        stop("info.txt is expected to have two columns")
    if (!all(colnames(x) == c("key", "description")))
        stop("columns of info.txt are expected to be called 'key' ",
             "and 'description'")
    if (!all(c("name", "description", "version", "date") %in% x[, "key"]))
        stop("info.txt is missing one or more required key entries")
    invisible(TRUE)
}

.labels <- function(x) {
    .read_dataset_file(x, "labels")
}

.valid_labels <- function(x) {
    if (!all(c("label", "type", "min", "max", "missing", "description") %in%
             colnames(x)))
        stop("labels.txt lacks one or more required columns.")
    if (any(is.na(x[, "label"])) | length(which(x[, "label"] == "")))
        stop("labels.txt: column 'label' contains missing or empty data")
    if (any(!x[, "type"] %in% c("label", "character", "integer", "categorical",
                                "float", "date", "time", "datetime",
                                "boolean")))
        stop("labels.txt: column 'type' contains unsupported values.")
    invisible(TRUE)
}

.mapping <- function(x) {
    .read_dataset_file(x, "mapping")
}

.valid_mapping <- function(x) {
    if (!all(c("label", "code", "value") %in% colnames(x)))
        stop("mapping.txt misses one or more required columns")
    invisible(TRUE)
}

#' Check that we have for all categorical variables the correct encodings.
#' 
#' @noRd
.valid_mapping_category_codes <- function(data, mapping) {
    maps <- split(mapping$code, map$label)
    for (variable in names(maps)) {
        uvals <- unique(data[, variable])
        if (!all(uvals) %in% maps[[variable]])
            stop("Categorical variable '", variable, "' has values without ",
                 "encoding.", call. = FALSE)
    }
}

#' Function to read the data, and format the columns correctly.

#' Function to read the annotation file for a specific data set.
