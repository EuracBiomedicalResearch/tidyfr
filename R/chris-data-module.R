#' @title CHRIS Data Modules
#'
#' @name DataModule
#'
#' @aliases DataModule-class
#'
#' @description
#'
#' A CHRIS Data Module provides the data of one specific module, which can be
#' the interview, clinical blood parameters or the metabolomics or proteomics
#' data sets. The actual data from a module is stored in the CHRIS Textual File
#' Format (CTFF - see [CTFF] for more details).
#'
#' The `chrisr` package represents a data module with the `DataModule` object
#' which provides all necessary functionality to import data of a module and to
#' format it properly for R.
#'
#' @section Loading a module:
#'
#' Available data modules in a certain path can be listed using the
#' [list_data_modules()] function.
#'
#' @section Accessing properties and data from a module:
#'
#' - `moduleName`: returns the name of a module.
#' - `modulePath`: returns the (full) file path to the data module.
#' - `moduleVersion`: returns the version of the data module.
#' - `moduleDescription`: returns the description of the module.
#' - `moduleDate`: returns the date of the module.
#'
#' @param object A `DataModule` object.
#'
#' @return See the individual function description.
#'
#' @author Johannes Rainer
#'
#' @exportClass DataModule
NULL

setClass("DataModule",
         slots = c(name = "character",
                   path = "character",
                   version = "character",
                   description = "character",
                   date = "character"),
         prototype = c(name = character(),
                       path = character(),
                       version = character(),
                       description = character(),
                       date = character()))

setValidity("DataModule", function(object) {
    msg <- character()
    if (length(modulePath(object)) && !dir.exists(modulePath(object)))
        msg <- c(msg, paste0("Module path \"", modulePath(object),
                             "\" does not exist."))
    if (!length(msg)) TRUE
    else msg
})

#' @rdname DataModule
#'
#' @export
moduleName <- function(object) object@name

#' @rdname DataModule
#'
#' @export
modulePath <- function(object) object@path

#' @rdname DataModule
#'
#' @export
moduleVersion <- function(object) object@version

#' @rdname DataModule
#'
#' @export
moduleDescription <- function(object) object@description

#' @rdname DataModule
#'
#' @export
moduleDate <- function(object) object@date

.valid_data_directory <- function(path, stop = FALSE) {
    fls <- dir(path)
    msgs <- character()
    if (!all(c("data.txt", "groups.txt", "grp_labels.txt",
               "info.txt", "labels.txt", "mapping.txt") %in% basename(fls))) {
        msgs <- c(msgs, paste0("Folder ", path, " is missing one or more",
                               " required data files."))
    }
    if (length(msgs)) {
        if (stop)
            stop(msgs)
        else return(msgs)
    }
    ## Test individual files; stop/return after each (because following tests
    ## depend on these).
    if (length(msgs <- .valid_info(.info(path), stop = stop))) return(msgs)
    data <- .data(path)
    if (length(msgs <- .valid_data(data, stop = stop))) return(msgs)
    labels <- .labels(path)
    if (length(msgs <- .valid_labels(labels, stop = stop))) return(msgs)
    if (length(msgs <- .valid_labels_data_types(labels, stop = stop)))
        return(msgs)
    if (length(msgs <- .valid_data_labels(data, labels, stop = stop)))
        return(msgs)
    mapping <- .mapping(path)
    if (length(msgs <- .valid_mapping(mapping, stop = stop))) return(msgs)
    if (length(msgs <- .valid_data_mapping_category_codes(data, mapping, stop)))
        return(msgs)
    if (length(msgs <- .valid_labels_mapping_categories(labels, mapping, stop)))
        return(msgs)
    groups <- .groups(path)
    if (length(msgs <- .valid_groups(groups, stop = stop))) return(msgs)
    if (length(msgs <- .valid_data_groups(data, groups, stop = stop)))
        return(msgs)
    grp_labels <- .grp_labels(path)
    if (length(msgs <- .valid_grp_labels(grp_labels, stop = stop))) return(msgs)
    if (length(msgs <- .valid_groups_grp_labels(
                   groups, grp_labels, stop = stop))) return(msgs)
    TRUE
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

.groups <- function(x) {
    .read_dataset_file(x, "groups")
}

.grp_labels <- function(x) {
    .read_dataset_file(x, "grp_labels")
}

.info <- function(x) {
    .read_dataset_file(x, "info")
}

.labels <- function(x) {
    .read_dataset_file(x, "labels")
}

.mapping <- function(x) {
    .read_dataset_file(x, "mapping")
}

#' Validator overview
#'
#' Format and content of individual data files:
#'
#' - `data` has column aid: `.valid_aid`
#' - `groups` has columns group and label: `.valid_groups`
#' - `grp_labels` has columns group and description: `.valid_grp_labels`
#' - `info` is correct: .valid_info
#' - `labels` has required columns: `.valid_labels`
#' - `mapping` has required columns label code and value: `.valid_mapping`.
#' - `labels` contains valid categories: `.valid_labels_data_types`
#'
#' Data consistency across data files:
#'
#' - `data` and `labels`: have an entry in labels for each data:
#'   `.valid_data_labels`.
#' - `data` and `mapping`: all categorical variables in `data` have
#'   corresponding categories in `mapping`: `.valid_data_mapping_category_codes`
#' - `labels` and `mapping`: we have encodings for all categorical variables:
#'   `.valid_labels_mapping_categories`
#' - `groups` and `data`: groups does not contain labels that are not in data:
#'   `.valid_data_groups`.
#' - `groups` and `grp_labels`: have a label for each group:
#'   `.valid_groups_grp_labels`.
#'
#' @noRd
NULL

#' data:
#' - column aid required
#' - columns contain supported data types
#'
#' @noRd
.valid_data <- function(x, stop = TRUE) {
    msgs <- character()
    if (!any(colnames(x) == "aid"))
        msgs <- c(msgs, "data lacks required column 'aid'")
    dtypes <- vapply(x, function(z) class(z)[1L], character(1))
    if (length(miss <- dtypes[!dtypes %in% names(.LABEL_DATA_TYPES)]))
        msgs <- c(msgs, paste0("data contains columns with unsupported data ",
                               "types: ", paste0(miss, collapse = ", ")))
    if (stop && length(msgs))
        stop(msgs)
    msgs
}

.valid_groups <- function(x, stop = TRUE) {
    msgs <- character()
    if (ncol(x) != 2)
        msgs <- c(msgs, "groups is expected to have two columns.")
    if (!length(msgs) && !all(colnames(x) == c("group", "label")))
        msgs <- c(msgs, "groups is required to have columns named ",
                  "\"group\" and \"label\".")
    if (stop && length(msgs))
        stop(msgs)
    msgs
}

.valid_grp_labels <- function(x, stop = TRUE) {
    msgs <- character()
    if (ncol(x) != 2)
        msgs <- c(msgs, "grp_labels is expected to have two columns.")
    if (!length(msgs) && !all(colnames(x) == c("group", "description")))
        msgs <- c(msgs, "grp_labels is required to have columns named ",
                  "\"group\" and \"description\".")
    if (stop && length(msgs))
        stop(msgs)
    msgs
}

.valid_info <- function(x, stop = TRUE) {
    msgs <- character()
    if (ncol(x) != 2)
        msgs <- c(msgs, "info is expected to have two columns")
    if (!length(msgs) && !all(colnames(x) == c("key", "description")))
        msgs <- c(msgs,
                  paste0("columns of info.txt are expected to be called 'key' ",
                         "and 'description'"))
    else if (!all(c("name", "description", "version", "date") %in% x[, "key"]))
        msgs <- c(msgs, "info is missing one or more required key entries")
    if (stop && length(msgs))
        stop(msgs)
    msgs
}

.valid_labels <- function(x, stop = TRUE) {
    msgs <- character()
    if (!all(c("label", "type", "min", "max", "missing", "description") %in%
             colnames(x)))
        msgs <- c(msgs, "labels lacks one or more required columns.")
    else {
        if (any(is.na(x[, "label"])) | length(which(x[, "label"] == "")))
            msgs <- c(msgs, paste0("labels: column 'label' contains",
                                   " missing or empty data"))
        if (any(!x[, "type"] %in% c("label", "character", "integer",
                                    "categorical", "float", "date", "time",
                                    "datetime", "boolean")))
            msgs <- c(
                msgs, "labels: column 'type' contains unsupported values.")
    }
    if (stop && length(msgs))
        stop(msgs)
    msgs
}

.valid_data_labels <- function(data, labels, stop = TRUE) {
    msgs <- character()
    if (any(miss <- !colnames(data) %in% c("aid", labels$label)))
        msgs <- c(msgs, paste0("missing labels for data column(s) ",
                               paste0(colnames(data)[miss], collapse = ", ")))
    if (stop && length(msgs))
        stop(msgs)
    msgs
}

.valid_mapping <- function(x, stop = TRUE) {
    msgs <- character()
    if (!ncol(x) == 3)
        msgs <- c(msgs, "mapping is expected to have 3 columns")
    if (!length(msgs) && !all(c("label", "code", "value") == colnames(x)))
        msgs <- c(msgs, paste0("mapping is required to have columns named ",
                               "\"label\", \"code\" and \"value\""))
    if (stop && length(msgs))
        stop(msgs)
    msgs
}

#' Check that we have for all categorical variables the correct encodings.
#'
#' @noRd
.valid_data_mapping_category_codes <- function(data, mapping, stop = TRUE) {
    msgs <- character()
    maps <- split(mapping$code, mapping$label)
    for (variable in names(maps)) {
        uvals <- as.integer(unique(data[, variable]))
        if (!all(uvals[!is.na(uvals)] %in% maps[[variable]]))
            msgs <- c(msgs, paste0("Categorical variable '", variable,
                                   "' has values without encoding."))
    }
    if (stop && length(msgs))
        stop(msgs)
    msgs
}

#' Check that we have mappings for all categorical variables
#'
#' @noRd
.valid_labels_mapping_categories <- function(labels, mapping, stop = TRUE) {
    msgs <- character()
    cats <- labels$label[labels$type == "categorical"]
    if (length(cats)) {
        miss <- which(!cats %in% mapping$label)
        if (length(miss))
            msgs <- c(msgs,
                      paste0("Missing encoding for categorical variables: ",
                             paste0("\"", cats[miss], "\"", collapse = ", ")))
    }
    if (stop && length(msgs))
        stop(msgs)
    msgs
}

.LABEL_DATA_TYPES <- c(numeric = "float", integer = "integer",
                       factor = "categorical", character = "character",
                       Date = "date", Time = "time", DateTime = "datetime",
                       logical = "boolean")
.valid_labels_data_types <- function(labels, stop = TRUE) {
    msgs <- character()
    if (nrow(labels)) {
        if (length(wrong <- labels$type[!labels$type %in% .LABEL_DATA_TYPES]))
            msgs <- c(msgs, paste0("labels contains unsupported data types: ",
                                   paste0("\"", wrong, "\"", collapse = ", ")))
    }
    if (stop && length(msgs))
        stop(msgs)
    msgs
}

#' groups does not contain labels that are not in data
#'
#' @noRd
.valid_data_groups <- function(data, groups, stop = TRUE) {
    msgs <- character()
    if (length(wrong <- groups$label[!groups$label %in% colnames(data)]))
        msgs <- c(msgs, paste0("groups contains labels that are not in data: ",
                               paste0("\"", wrong, "\"", collapse = ", ")))
    if (stop && length(msgs))
        stop(msgs)
    msgs
}

.valid_groups_grp_labels <- function(groups, grp_labels, stop = TRUE) {
    msgs <- character()
    if (length(miss <- groups$group[!groups$group %in% grp_labels$group]))
        msgs <- c(msgs, paste0("missing group descriptions in grp_labels for: ",
                               paste0("\"", miss, "\"", collapse = ", ")))
    if (stop && length(msgs))
        stop(msgs)
    msgs
}
