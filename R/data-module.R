#' @title Data Modules
#'
#' @name DataModule
#'
#' @aliases DataModule-class show,DataModule-method
#'
#' @description
#'
#' A Data Module provides the data of one specific module, which can be
#' the interview, clinical blood parameters or the metabolomics or proteomics
#' data sets. The actual data from a module is stored in the Textual Dataset
#' Format (TDF - see [TDF] for more details).
#'
#' The `tidyfr` package represents a data module with the `DataModule` object
#' which provides all necessary functionality to import data of a module and to
#' format it properly for R.
#'
#' @section Loading a module:
#'
#' Available data modules in a certain path can be listed using the
#' [list_data_modules()] function.
#'
#' - `data_module`: load a specific data module. The name and version of the
#'   data module to load needs to be specified with parameters `name` and
#'   `version` respectively. Parameter `path` can be used to set the base path
#'   where the data module can be found. The function returns an instance of
#'   `DataModule`.
#'
#' @section Accessing properties and data from a module:
#'
#' - `data`: returns the data of a module as a `data.frame`. By default (with
#'   parameter `aidAsRownames = TRUE`) *aid*s provided by the data module
#'   will be used as row names for the returned `data.frame`.
#'   For `aidAsRownames = FALSE` or if the aids provided by the module are not
#'   unique, a column `"aid"` is added (as first column) to the returned
#'   `data.frame`.
#'   Columns (variables) in the returned `data.frame` are correctly formatted
#'   (i.e. as `factors`, `integers`, `numeric`, `character` or date/time
#'   formats) according to the *labels* information of the data module. Use
#'   the `labels` function to retrieve variable information (annotation)
#'   from the data module.
#'
#' - `groups`: returns a `data.frame` with the optional grouping of variables.
#'   The group descriptions are provided byt the `grp_labels` function.
#'
#' - `grp_labels`: returns a `data.frame` with a description for each defined
#'   variable group.
#'
#' - `labels`: returns a `data.frame` with the description and annotation of the
#'   individual variables (labels).
#'
#' - `moduleName`: returns the name of a module.
#'
#' - `modulePath`: returns the (full) file path to the data module.
#'
#' - `moduleVersion`: returns the version of the data module.
#'
#' - `moduleDescription`: returns the description of the module.
#'
#' - `moduleDate`: returns the date of the module.
#'
#' @section Managing data modules:
#'
#' Data maintainers can use functions listed here to *manage* existing data
#' resources. Alternatively, see also [export_tdf()] for information how to
#' create new data modules in TDF format.
#'
#' - [remove_participants()]: create a new version of the current module by
#'   removing participant data for individuals with the specified aids.
#'
#' @param aidAsRownames optional parameter for `data`: if `TRUE` (the default)
#'     the AIDs provided by the data module are used as row names of the
#'     returned `data.frame` (unless they are not unique).
#'
#' @param base For `modulePath`: `logical(1)` whether the base folder or the
#'     actual data folder should be returned. The *base* folder (returned with
#'     `base = TRUE`) is the folder of the module containing eventual multiple
#'     versions of it. The data folder (returned with `base = FALSE`, default)
#'     is the actual folder containing the data for the selected version of
#'     the module.
#'
#' @param name For `data_module`: `character(1)` defining the name of the
#'     module to load.
#'
#' @param object A `DataModule` object.
#'
#' @param path For `data_module`: `character(1)` defining the path where data
#'     modules are stored.
#'
#' @param version For `data_module`: `character(1)` defining the version of the
#'     module to load.
#'
#' @param ... For `data`: additional arguments.
#'
#' @return See the individual function description.
#'
#' @author Johannes Rainer
#'
#' @exportClass DataModule
#'
#' @examples
#'
#' ## List available test data modules provided by the tidyfr package
#' pth <- system.file("txt", package = "tidyfr")
#' list_data_modules(pth)
#'
#' ## Load one data module
#' mdl <- data_module(name = "db_example2", version = "1.0.1", path = pth)
#' mdl
#'
#' ## Get the name, description and version of the module
#' moduleName(mdl)
#' moduleDescription(mdl)
#' moduleVersion(mdl)
#'
#' ## Get the data from the module
#' d <- data(mdl)
#' d
#'
#' ## Variables are correctly formatted:
#' ## categorical variables (factors):
#' d$x0_sex
#'
#' ## Dates:
#' d$x0_examd
#'
#' ## Numeric:
#' d$x0_age
#'
#' ## Get information on all variables
#' labels(mdl)
#'
#' ## Get information of variable grouping
#' groups(mdl)
#'
#' ## Get the corresponding group description
#' grp_labels(mdl)
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

#'@rdname DataModule
#'
#'@importFrom methods new
#'
#' @export
data_module <- function(name = character(), version = character(),
                        path = data_path()) {
    if (!length(name))
        stop("'name' needs to be specified")
    if (!length(version))
        stop("'version' needs to be specified")
    module_path <- file.path(path, name, version, "data")
    if (!dir.exists(module_path))
        stop("No data module with that name and version exists in '", path, "'")
    .valid_data_directory(module_path, stop = TRUE)
    minfo <- .info(module_path)
    new("DataModule",
        name = minfo$description[minfo$key == "name"],
        path = module_path,
        version = minfo$description[minfo$key == "version"],
        description = minfo$description[minfo$key == "description"],
        date = minfo$description[minfo$key == "date"])
}

#' @exportMethod show
#'
#' @importMethodsFrom methods show
setMethod("show", "DataModule", function(object) {
    cat("Object of class", class(object), "\n")
    cat(" o name:\t", moduleName(object), "\n", sep = "")
    cat(" o version:\t", moduleVersion(object), "\n", sep = "")
    cat(" o description:\t", moduleDescription(object), "\n", sep = "")
    cat(" o date:\t", moduleDate(object), "\n", sep = "")
})

#' @rdname DataModule
#'
#' @export
grp_labels <- function(object) {
    validObject(object)
    .grp_labels(modulePath(object))
}

#' @rdname DataModule
#'
#' @export
setMethod("labels", "DataModule", function(object) {
    validObject(object)
    .labels(modulePath(object))
})

#' @rdname DataModule
#'
#' @export
setMethod("groups", "DataModule", function(object) {
    validObject(object)
    .groups(modulePath(object))
})

#' @exportS3Method tidyfr::groups
groups.DataModule <- function(object){
  validObject(object)
  .groups(modulePath(object))
  
}

#' @rdname DataModule
#'
#' @export
moduleName <- function(object) object@name

#' @rdname DataModule
#'
#' @export
modulePath <- function(object, base = FALSE) {
    p <- object@path
    if (base)
        p <- .path_up(p, 2L)
    p
}

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

#' The *main* validator for a module. Checks that all files are available,
#' that all files have expected content and that information across files is
#' coherent
#'
#' @noRd
.valid_data_directory <- function(path, stop = FALSE, quick = FALSE) {
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
    if (!quick) {
        ## Test individual files; stop/return after each (because following
        ## tests depend on these).
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
        mapping_categorical <- mapping[
            mapping$label %in% labels$label[labels$type == "categorical"], ,
            drop = FALSE]
        if (length(msgs <- .valid_data_mapping_category_codes(
                       data, mapping_categorical, stop)))
            return(msgs)
        if (length(msgs <- .valid_labels_mapping_categories(
                       labels, mapping, stop)))
            return(msgs)
        groups <- .groups(path)
        if (length(msgs <- .valid_groups(groups, stop = stop))) return(msgs)
        if (length(msgs <- .valid_data_groups(data, groups, stop = stop)))
            return(msgs)
        grp_labels <- .grp_labels(path)
        if (length(msgs <- .valid_grp_labels(grp_labels, stop = stop)))
            return(msgs)
        if (length(msgs <- .valid_groups_grp_labels(
                       groups, grp_labels, stop = stop))) return(msgs)
    }
    TRUE
}

#' @importFrom utils read.table
#'
#' @noRd
.read_dataset_file <- function(x, name) {
    read.table(paste0(file.path(x, name), ".txt"),
               sep = "\t", header = TRUE, quote = "", comment.char = "")
}

.data <- function(x) {
    .read_dataset_file(x, "data")
}

.groups <- function(x) {
    .read_dataset_file(x, "groups")
}

.grp_labels <- function(x) {
    gl <- .read_dataset_file(x, "grp_labels")
    rownames(gl) <- gl$group
    gl
}

.info <- function(x) {
    .read_dataset_file(x, "info")
}

.labels <- function(x) {
    l <- .read_dataset_file(x, "labels")
    rownames(l) <- l$label
    if (file.exists(file.path(x, "labels_additional_info.txt"))) {
        l_a <- .read_dataset_file(x, "labels_additional_info")
        rownames(l_a) <- l_a$label
        l_a <- l_a[, !colnames(l_a) == "label", drop = FALSE]
        if (!all(rownames(l) %in% rownames(l_a)))
            warning("Ignoring file \"labels_additional_info.txt\" ",
                    "because it is in a wrong format.")
        else l <- cbind(l, l_a[rownames(l), , drop = FALSE])
    }
    l
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

#' @title Remove data for selected participants
#'
#' @description
#'
#' The `remove_participants` function creates a new version of an existing
#' data module removing any data for the selected individuals (parameter
#' `aid`). The function will:
#'
#' - bump the version of the module (incrementing the 4th position in the
#'   *x.y.z.a* version scheme).
#' - create a new folder (name being the new version) in the directory `path`.
#' - copy all files from the original module (version) to the new folder.
#' - remove all entries (rows) in the *data.txt* files with their `aid` label
#'   matching `aid`.
#' - validate the new data module.
#'
#' @param x `DataModule` from which data of a participant should be removed.
#'
#' @param aid `character` with the IDs (label `"aid"`) of the participants for
#'     whom data should be removed. Note that [format_aid()] will be called
#'     on this parameter to ensure the format of the AID matches.
#'
#' @param path `character(1)` defining the path where the new version of the
#'     data set should be stored into. Defaults to the base path of the
#'     current module.
#'
#' @return `character(1)` with the path to the folder containing the new
#'     version.
#'
#' @author Johannes Rainer
#'
#' @export
remove_participants <- function(x, aid, path = modulePath(x, base = TRUE)) {
    new_version <- .bump_version(moduleVersion(x))
    ov_path <- .path_up(modulePath(x), 1)
    nv_path <- file.path(path, new_version)
    if (dir.exists(nv_path))
        stop("Version ", new_version, " does already exist in ", path)
    dir.create(nv_path)
    fls <- list.files(path = ov_path, full.names = TRUE)
    file.copy(fls, recursive = TRUE, to = nv_path, copy.date = TRUE)
    dta <- .data(file.path(nv_path, "data"))
    if (!any(colnames(dta) == "aid"))
        stop("data does not contain the required column \"aid\".")
    dta$aid <- format_aid(dta$aid)
    aid <- format_aid(aid)
    rem <- dta$aid %in% aid
    .export_data(dta[!rem, , drop = FALSE], path = file.path(nv_path, "data"))
    message("Removed data for ", sum(rem), " individuals.")
    .valid_data_directory(file.path(nv_path, "data") , stop = TRUE)
    message("New version created. You might now want to check the directory\n",
            nv_path, "\nand update/fix documentation, NEWS and description.")
    nv_path
}
