#' @title Export data in the CHRIS Textual File Format
#'
#' The `export_ctff` exports the provided data in the CHRIS textual file format
#' (ctff). The function first creates all required folders, checks the input
#' files and exports the data in the ctff format.
#'
#' Automatic convertions performed by the function are:
#'
#' - Columns in `data` that are of data type `factor` are correctly and
#'   automatically converted (i.e. their categories added to the `mapping`
#'   `data.frame`.
#' - If not specified in `labels`, columns `"min"`, `"max"` in `labels` are
#'   calculated on the provided `data`.
#' - Missing values in `data` are automatically converted and the respective
#'   encoding specified in `labels`.
#'
#' See the official
#' [CTFF](https://wiki.gm.eurac.edu/index.php?title=Textual_Dataset_Format)
#' definition on the expected format of the data.
#'
#' @param name **required** `character(1)` with the name of the data module.
#'
#' @param description `character(1)` providing a description of the data.
#'
#' @param version **required** `character(1)` with the version of the data.
#'
#' @param date `character(1)` providing the date of the data.
#'
#' @param path `character(1)` with the base path where the folders and data
#'     files should be created. Defaults to `path = "."`.
#'
#' @param data `data.frame` with the data to export. Required column `"aid"` is
#'     expected to contain the unique identifiers of the participants. All
#'     additional columns are expected to contain the data of additional
#'     variables.
#'
#' @param groups `data.frame` with optional grouping of labels (variables) in
#'     `data`. Expected columns are `"group"` and `"label"`. See the ctff
#'     definition for details.
#'
#' @param grp_labels `data.frame` with the names (descriptions) of the groups
#'     defined in `groups`.
#'
#' @param labels `data.frame` with *annotations* to the variables (labels) in
#'     `data`. See the ctff definition for details. Columns `"min"`, `"max"`
#'     and `"missing"` will be filled by the `export_ctff` function.
#'
#' @param mapping `data.frame` with the definition of the levels (categories)
#'     of the categorical variables in `labels`. Expected columns are
#'     `"label"`, `"code"` and `"value"`.
#'
#' @return (invisibly) `character(1)` with the path to the folder where the
#'     data was stored.
#'
#' @author Johannes Rainer
#'
#' @export
export_ctff <- function(name = character(), description = character(),
                       version = character(), date = character(),
                       path = ".", data = data.frame(), groups = data.frame(),
                       grp_labels = data.frame(), labels = data.frame(),
                       mapping = data.frame()) {
    if (!length(name))
        stop("'name' has to be defined")
    if (!length(version))
        stop("'version' has to be defined")
    module_path <- file.path(path, name)
    if (!dir.exists(module_path)) dir.create(module_path)
    module_path <- file.path(module_path, version)
    if (!dir.exists(module_path)) dir.create(module_path)
    module_path <- file.path(module_path, "data")
    if (!dir.exists(module_path)) dir.create(module_path)
    info_file <- file.path(module_path, "info.txt")
    if (file.exists(info_file))
        stop("A data set with the specified name and version does ",
             "already exist")
    ## Checking and exporting...
    if (nrow(data))
        .valid_data(data, stop = TRUE)
    else data <- .empty_data()
    if (nrow(groups))
        .valid_groups(groups, stop = TRUE)
    else groups <- .empty_groups()
    if (nrow(grp_labels))
        .valid_grp_labels(grp_labels, stop = TRUE)
    else grp_labels <- .empty_grp_labels()
    if (nrow(labels)) {
        ## Check if columns min, max and missing are there...
        minmax <- .data_min_max(data)
        minmax <- minmax[rownames(minmax) != "aid", ]
        if (!any(colnames(labels) == "missing"))
            labels$missing[vapply(data, is.numeric, logical(1))] <- na
        if (!any(colnames(labels) == "min"))
            labels$min <- minmax$min
        if (!any(colnames(labels) == "max"))
            labels$max <- minmax$max
    }
    labels <- .fill_labels(labels)
    .valid_labels(labels, stop = TRUE)
    .valid_labels_data_types(labels, stop = TRUE)
    if (nrow(mapping))
        .valid_mapping(mapping, stop = TRUE)
    else mapping <- .empty_mapping()
    ## Check for correct data content.
    .valid_data_labels(data, labels, stop = TRUE)
    .valid_data_mapping_category_codes(data, mapping, stop = TRUE)
    .valid_labels_mapping_categories(labels, mapping, stop = TRUE)
    .valid_data_groups(data, groups, stop = TRUE)
    .valid_groups_grp_labels(groups, grp_labels, stop = TRUE)
    ## Actual exporting
    .info_skeleton(name = name, description = description,
                   version = version, date = date, path = module_path)
    ## Format and update/fix the data.
    ## - data: replace missing values.
    ## - labels: add minimum, maximum and NA encoding.
    na <- -89
    data <- .replace_na_data(data, na = na)
    ## categorical/factor
    col_type <- vapply(data, class, character(1))
    idx <- which(col_type == "factor")
    for (i in idx) {
        lvls <- levels(data[, i])
        data[, i] <- .format_categorical_export(data[, i])
        mapping <- rbind(mapping, data.frame(label = colnames(data)[i],
                                             code = seq_along(lvls),
                                             value = lvls))
    }
    ## date?
    .export_data(data, path = module_path)
    .export_groups(groups, path = module_path)
    .export_grp_labels(grp_labels, path = module_path)
    .export_labels(labels, path = module_path)
    .export_mapping(mapping, path = module_path)
    message("Data set was written to: ", module_path, "\n")
    invisible(module_path)
}

#' Format the data for export. This means replacing missing values with with
#' the value specified with parameter `na`.
#'
#' @param data `data.frame
#'
#' @author Johannes Rainer
#'
#' @noRd
.replace_na_data <- function(data, na = -89) {
    as.data.frame(lapply(data, function(z) {
        z[is.na(z)] <- na
        z
    }))
}

#' Get the minimum and maximum (non-NA) value for each column in `data`.
#'
#' @author Johannes Rainer
#'
#' @noRd
.data_min_max <- function(data) {
    do.call(rbind, lapply(data, function(z) {
        if (is.numeric(z))
            data.frame(min = min(z, na.rm = TRUE), max = max(z, na.rm = TRUE))
        else data.frame(min = NA, max = NA)
        }))
}

.empty_data <- function() {
    data.frame(aid = character())
}

.empty_mapping <- function() {
    data.frame(label = character(), code = character(), value = character())
}

.empty_groups <- function() {
    data.frame(group = character(), label = character())
}

.empty_grp_labels <- function() {
    data.frame(group = character(), description = character())
}

.labels_from_date <- function(data, na = -89) {
    ## Create an empty labels data.frame from a `data` data.frame
    dty <- vapply(data, class, character(1))
    labels <- data.frame(label = colnames(data), )
}

.fill_labels <- function(labels = data.frame()) {
    cols <- c("label", "unit", "type", "min", "max", "missing", "description")
    for (col in cols[!cols %in% colnames(labels)]) {
        labels[[col]] <- rep("", nrow(labels))
    }
    add_cols <- setdiff(colnames(labels), cols)
    labels[, c(cols, add_cols)]
}

.info_skeleton <- function(name = "", description = "", version = "",
                           date = "", path = ".") {
    out <- paste0("key\tdescription\n",
                  "name\t", name, "\n",
                  "description\t", description, "\n",
                  "version\t", version, "\n",
                  "date\t", date, "\n")
    writeLines(out, con = file.path(path, "info.txt"))
}

#' @importFrom utils write.table
.export_data <- function(path = ".", data = data.frame()) {
    write.table(data, sep = "\t", quote = FALSE, row.names = FALSE,
                file = file.path(path, "data.txt"))
}

.export_mapping <- function(path = ".", mapping = .empty_mapping()) {
    write.table(mapping, sep = "\t", quote = FALSE,
                row.names = FALSE, file = file.path(path, "mapping.txt"))
}

#' @param labels `data.frame` with columns that should be exported.
#'
#' @noRd
.export_labels <- function(path = ".", labels = data.frame()) {
    write.table(.fill_labels(labels), sep = "\t", quote = FALSE,
                row.names = FALSE, file = file.path(path, "labels.txt"))
}

.export_groups <- function(path = ".", groups = .empty_groups()) {
    write.table(groups, sep = "\t", quote = FALSE,
                row.names = FALSE, file = file.path(path, "groups.txt"))
}

.export_grp_labels <- function(path = ".", grp_labels = .empty_grp_labels()) {
    write.table(grp_labels, sep = "\t", quote = FALSE,
                row.names = FALSE, file = file.path(path, "grp_labels.txt"))
}
