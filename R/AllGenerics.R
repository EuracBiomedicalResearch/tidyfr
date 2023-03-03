setGeneric("labels", function(object, ...)
    standardGeneric("labels"))
setGeneric("groups", function(object, ...)
    standardGeneric("groups"))

#' @title Textual Dataset Format
#'
#' @aliases labels groups
#'
#' @description
#'
#' The `tidyfr` package provides convenience and utility functions to import
#' data stored in the new *Textual Dateset Format* into R. These
#' functions ensure the data is properly formatted which include the correct
#' encoding categorical variables (`factor`s) or also missing values.
#'
#' TDF data is structured in the following format:
#'
#' - **data**: contains the data of the various variables. Columns are
#'   variables, rows study participants. Column `"aid"` contains the ID of the
#'   study participants.
#'
#' - **labels**: provides information on the variables in *data*. Columns are
#'   `"label"` (the name of the column in *data*), `"unit"` (unit of the
#'   measured value), `"type"` (the data type), `"min"` (the minimal value),
#'   `"max"` (the maximal value), `"missing"` (the value with which missing
#'   values in *data* are encoded) and `"description"` (a name/description of
#'   the variable). Optional additional columns (annotations) might be
#'   available depending on the data module.
#'
#' - **groups**: provides optional grouping of variables in *data*.
#'
#' - **grp_labels**: contains descriptions for the *groups*.
#'
#' See the official
#' [TDF](https://wiki.gm.eurac.edu/index.php?title=Textual_Dataset_Format)
#' definition for a complete description of the format.
#'
#' @section Accessing Data:
#'
#' The following functions can be used to import CHRIS data from one of the
#' available data modules.
#'
#' - `data`: retrieves the *data*.
#'
#' - `groups`: retrieves the grouping information for variables.
#'
#' - `labels`: retrieves additional annotation/information for specific
#'   variables of a module.
#'
#' Which data is retrieved depends on the parameter `object`.
#' See [chris-SummarizedExperiment] if `object` is a [SummarizedExperiment()].
#'
#' @author Johannes Rainer
#'
#' @name TDF
NULL

#' @importFrom methods is validObject
#'
#' @export data
#'
#' @rdname DataModule
data <- function(..., aidAsRownames = TRUE) {
    vars <- list(...)
    if (length(vars) && inherits(vars[[1L]], "SummarizedExperiment")) {
        names(vars)[1L] <- "x"
        return(do.call(.data_from_SummarizedExperiment, vars, quote = TRUE))
    }
    if (length(vars) && is(vars[[1L]], "DataModule")) {
        return(.data_import(modulePath(vars[[1L]]),
                            aidAsRownames = aidAsRownames))
    }
    utils::data(...)
}
