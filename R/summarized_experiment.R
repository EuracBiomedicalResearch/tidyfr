#' Functions and methods to import/export data from/to SummarizedExperiment objects.
#'
#' @noRd
NULL

#' @importFrom SummarizedExperiment assay assayNames
#'
#' @param x `SummarizedExperiment`.
#'
#' @param assayNames. `character` with the names of the assays to export.
#'
#' @param label_prefix `character` with the *prefix* to be used to create the
#'     labels. The final label will be the prefix and a number.
#'
#' @return `data.frame` with the data from the assays of `x` as columns.
#'     Labels will be used as column names with a character from the alphabet
#'     appended to the label for each assay different than the first.
#'
#' @noRd
.data_from_SummarizedExperiment <- function(x, assayNames. = assayNames(x),
                                            label_prefix = "x0xx") {
    label <- paste0(label_prefix, .integer_string(seq_len(nrow(x))))
    if (is.null(aid <- colnames(x)))
        aid <- seq_len(ncol(x))
    res <- data.frame(aid = aid)
    la <- length(assayNames.)
    suff <- c("", letters[seq_len(la - 1L)])
    for (i in seq_len(la)) {
        tmp <- as.data.frame(t(assay(x, assayNames.[i])))
        colnames(tmp) <- paste0(label, suff[i])
        res <- cbind(res, tmp)
    }
    res
}

.groups_from_SummarizedExperiment <- function(x, assayNames. = assayNames(x),
                                              label_prefix = "x0xx") {
    grps <- paste0("assay_", assayNames.)
    label <- paste0(label_prefix, .integer_string(seq_len(nrow(x))))
    la <- length(assayNames.)
    suff <- c("", letters[seq_len(la - 1L)])
    data.frame(group = c(rep(grps, each = length(label)),
                         rep(paste0("analyte_", label), each = length(suff))),
               label = c(paste0(rep(label, length(suff)),
                                rep(suff, each = length(label))),
                         paste0(rep(label, each = length(suff)),
                                rep(suff, length(label)))))

}

.integer_string <- function(x) {
    sprintf(paste0("%0", ceiling(log10(max(x) + 1L)), "d"), x)
}

#' @importMethodsFrom SummarizedExperiment rowData
.labels_from_SummarizedExperiment <- function(x, assayNames. = assayNames(x),
                                              label_prefix = "x0xx") {
    d <- .data_from_SummarizedExperiment(x, assayNames. = assayNames.,
                                         label_prefix = label_prefix)
    l <- labels_from_data(d)
    an <- ""
    al <- length(assayNames.)
    if (al > 1L)
        an <- c(an, paste0(" assay ", assayNames.[-1L]))
    l$description <- paste0(rep(rownames(x), length(an)),
                            rep(an, each = nrow(x)))
    ## Combine with rowData. But put content ONLY to the first x rows.
    rd <- rd_full <- as.data.frame(rowData(x))
    ## rd[,] <- NA
    for (i in seq_len((al - 1L)))
        rd_full <- rbind(rd_full, rd)
    cbind(l, rd_full)
}

#' @title Extracting CTFF-compliant data from a SummarizedExperiment
#'
#' @name chris-SummarizedExperiment
#'
#' @aliases chris_data,SummarizedExperiment-method chris_labels,SummarizedExperiment-method chris_groups,SummarizedExperiment-method
#'
#' @description
#'
#' The [SummarizedExperiment()] class is a container for data from large scale
#' assays for biological experiments. In contrast to CHRIS data, samples are
#' organized in columns of a `SummarizedExperiment` and measurements in rows.
#' The `data`, `labels` and `groups` methods allow to extract information from
#' such objects in a CHRIS Textual File Format (CTFF)-compliant format
#' (structure):
#'
#' - columns are variables, rows individuals (samples).
#' - variable IDs (labels) follow a standard format (e.g. `"x0pt001"`).
#'
#' The available methods are:
#'
#' - `chris_data`: export the (quantitative) assay data from an
#'   `SummarizedExperiment` as a `data.frame` with columns representing
#'   variables and rows samples (study participants). Parameter `assayNames.`
#'   allows to specify which of the assays from the `SummarizedExperiment`
#'   should be extracted. Variables (rows) in the `SummarizedExperiment` get
#'   assigned a new variable ID (called *label*), which consists of the
#'   `labelPrefix` followed by an integer representing the index of the
#'   variable in the `SummarizedExperiment` (i.e. the row number of the
#'   variable in the `SummarizedExperiment`). A letter is appended to IDs for
#'   variables from assays different than the first one. Thus, `"x0xx001"`
#'   corresponds to the first row in the first assay, while `"x0xx001a"`
#'   represents the first row in the second assay. The `colnames` of the
#'   `SummarizedExperiment` are used as sample identifiers and are returned in
#'   column `"aid"` of the result `data.frame`.
#'
#' - `chris_groups`: retrieves a `data.frame` that specifies the grouping of
#'   variables returned by `data` from a `SummarizedExperiment`. Columns
#'   (variables) containing data from the same `assay` of the
#'   `SummarizedExperiment` are grouped into the same group.
#'
#' - `chris_labels`: extracts label annotations for the data extracted with
#'   `data` from a `SummarizedExperiment`. The returned `data.frame` is in the
#'   *labels* format of the CTFF but contains additional columns with the
#'   available annotations from the `SummarizedExperiment`'s [rowData()]. The
#'   `rownames` of the `SummarizedExperiment` are returned in columns
#'   `"description"`.
#'
#' @param object A `SummarizedExperiment` object.
#'
#' @param assayNames. `character` defining the names of the *assays* in
#'     `object` from which data should be extracted.
#'
#' @param labelPrefix `character(1)` defining the prefix for the variable IDs
#'     (labels) of the object.
#'
#' @return a `data.frame` with the data.
#'
#' @rdname chris-SummarizedExperiment
#'
#' @importFrom SummarizedExperiment SummarizedExperiment
#'
#' @author Johannes Rainer
#'
#' @examples
#'
#' ## Create a simple SummarizedExperiment with some random data as one assay
#' ## and a second assay with all values multiplied with 2. For a
#' ## SummarizedExperiment columns represent samples and rows measurements
#' ## (variables).
#' mat <- matrix(rnorm(60), ncol = 10, nrow = 6)
#'
#' ## SummarizedExperiments allow to store also column and row annotations along
#' ## with the data. We thus define below a data.frame with some annotations
#' ## for the variables.
#' rowd <- data.frame(analyte_id = paste0("id", 1:6), analyte_name = letters[1:6])
#' rownames(rowd) <- rowd$analyte_id
#'
#' library(SummarizedExperiment)
#' se <- SummarizedExperiment(
#'     assay = list(values = mat, double = 2 * mat),
#'     rowData = rowd)
#' se
#'
#' ## What assays are available?
#' assayNames(se)
#'
#' ## Get a data.frame with all variables
#' chris_data(se)
#'
#' ## Get the label information
#' chris_labels(se)
#'
#' ## Get the variable grouping
#' chris_groups(se)
NULL

#' @rdname chris-SummarizedExperiment
#'
#' @export
setMethod("chris_data", "SummarizedExperiment",
          function(object, assayNames. = assayNames(object),
                   labelPrefix = "x0xx") {
              .data_from_SummarizedExperiment(object, assayNames. = assayNames.,
                                              label_prefix = labelPrefix)
          })

#' @rdname chris-SummarizedExperiment
#'
#' @export
setMethod("chris_groups", "SummarizedExperiment",
          function(object, assayNames. = assayNames(object),
                   labelPrefix = "x0xx") {
              .groups_from_SummarizedExperiment(
                  object, assayNames. = assayNames., label_prefix = labelPrefix)
          })

#' @rdname chris-SummarizedExperiment
#'
#' @export
setMethod("chris_labels", "SummarizedExperiment",
          function(object, assayNames. = assayNames(object),
                   labelPrefix = "x0xx") {
              .labels_from_SummarizedExperiment(
                  object, assayNames. = assayNames., label_prefix = labelPrefix)
          })
