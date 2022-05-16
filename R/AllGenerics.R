setGeneric("chris_data", function(object, ...)
    standardGeneric("chris_data"))
setGeneric("chris_labels", function(object, ...)
    standardGeneric("chris_labels"))
setGeneric("chris_groups", function(object, ...)
    standardGeneric("chris_groups"))

#' @title CHRIS Textual File Format Data
#'
#' @aliases chris_data chris_labels chris_groups
#'
#' @description
#'
#' The `chrisr` package provides convenience and utility functions to import
#' CHRIS data stored in the new *CHRIS Textual File Format* into R. These
#' functions ensure the data is properly formatted which include the correct
#' encoding categorical variables (`factor`s) or also missing values.
#'
#' CHRIS Data is structured in the following format:
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
#' [CTFF](https://wiki.gm.eurac.edu/index.php?title=Textual_Dataset_Format)
#' definition for a complete description of the format.
#'
#' @section Accessing CHRIS Data:
#'
#' The following functions can be used to import CHRIS data from one of the
#' available data modules.
#'
#' - `chris_data`: retrieves the *data*.
#'
#' - `chris_groups`: retrieves the grouping information for variables.
#'
#' - `chris_labels`: retrieves additional annotation/information for specific
#'   variables of a CHRIS module.
#'
#' Which data is retrieved depends on the parameter `object`.
#' See [chris-SummarizedExperiment] if `object` is a [SummarizedExperiment()].
#'
#' @author Johannes Rainer
#'
#' @name CTFF
NULL