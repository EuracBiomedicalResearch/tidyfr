#' @title Export data in the Textual Dataset File Format
#'
#' @aliases labels_from_data mapping_from_data
#'
#' @description
#'
#' The `export_tdf` exports the provided data in the TDFF format. The function
#' first creates all required folders, checks the input
#' files and then exports the data in the TDFF format (see below for more
#' information on this format).
#'
#' The data is organized in the following way:
#'
#' - Within the base directory `path` a folder `name` is created for the
#'   data set.
#' - Within a folder with the version of the data set (parameter `version`)
#'   two folders *data* and *docs* are created. The actual data files
#'   are stored in the *data* folder while the *docs* folder allows to
#'   contains any documentation files (any file) related to the data set. The
#'   *docs* folder contains also a file *docs.txt* that is supposed to
#'   contain information for any added documentation file (this information
#'   needs to be manually addedd).
#' - Within the base folder (with the name of the data set) a *NEWS.md* file
#'   is created which is supposed to be manually edited to add some
#'   information or change log for the currently exported version of the
#'   data.
#'
#' Automatic convertions performed by the export function are:
#'
#' - Columns in `data` that are of data type `factor` are correctly and
#'   automatically converted to the expected format (i.e. their categories are
#'   added to the `mapping` `data.frame` and the values are replaced with the
#'   indices).
#' - If not specified in `labels`, columns `"min"`, `"max"` in `labels` are
#'   calculated on the provided `data`.
#' - Missing values in `data` are automatically converted and the respective
#'   encoding specified in `labels`.
#'
#' The `labels_from_data` creates a *template* *labels* `data.frame` from the
#' provided `data`. The function retrieves various information like the data
#' type of the various columns from the provided `data` and adds the
#' corresponding values to the `data.frame`. Other columns, such as
#' `"description"` or `"unit"` need to be filled out manually.
#'
#' The `mapping_from_data` creates a *mapping* `data.frame` for all categorical
#' variables in `data` (i.e. columns in `data` with data type `factor`).
#'
#' @section Short information on the TDF:
#'
#' See the official
#' [Textual Dataset Format](https://wiki.gm.eurac.edu/index.php?title=Textual_Dataset_Format)
#' definition for a complete description of the format.
#'
#' - **data**: contains the data of the various variables. Columns are
#'   variables, rows study participants. Column `"aid"` is mandatory and
#'   contains the ID of the study participants.
#'
#' - **labels**: provides information on the variables in *data*. Columns are
#'   `"label"` (the name of the column in *data*), `"unit"` (unit of the
#'   measured value), `"type"` (the data type), `"min"` (the minimal value),
#'   `"max"` (the maximal value), `"missing"` (the value with which missing
#'   values in *data* are encoded) and `"description"` (a name/description of
#'   the variable).
#'
#' - **mapping**: contains the encoding of categorical variables (`factor`s) in
#'   *data*. Required columns are: `"label"` (the name of the column in *data*),
#'   `"code"` (the value of the category in *data*) and `"value"` (the category,
#'   i.e. the `level` of the `factor`).
#'
#' - **groups**: allows to optionally group variables in *data*. Expected
#'   columns are `"group"` (the name of the group) and `"label"` (the name of
#'   the column in *data*).
#'
#' - **group_labels**: contains descriptions for the *groups*. Expected columns
#'   are `"group"` (the name of the group) and `"description"` (the
#'   name/description of the group).
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
#'     `data`. Expected columns are `"group"` and `"label"`. See the TDF
#'     definition for details.
#'
#' @param group_labels `data.frame` with the names (descriptions) of the groups
#'     defined in `groups`.
#'
#' @param labels `data.frame` with *annotations* to the variables (labels) in
#'     `data`. See the TDF definition for details. Columns `"min"`, `"max"`
#'     and `"missing"` will be filled by the `export_tdf` function if not
#'     already provided. Defaults to `labels = labels_from_data()` hence
#'     creates a *labels* `data.frame` from the provided `data`. Any
#'     annotations that are not part of the pre-defined hard set of
#'     columns are stored in a separate file *labels_additional_info.txt*.
#'
#' @param mapping `data.frame` with the definition of the levels (categories)
#'     of the categorical variables in `labels`. Expected columns are
#'     `"label"`, `"code"` and `"value"`. The default is
#'     `mapping = mapping_from_data(data)` and a *mapping* `data.frame` is thus
#'     generated by default from the provided `data`.
#'
#' @param na the value to represent missing values in `data`.
#'
#' @return `export_tdf`: (invisibly) returns a `character(1)` with the path to
#'     the folder where the data was stored. `labels_from_data` returns a
#'     *labels* `data.frame` based on the data in `data`.
#'
#' @author Johannes Rainer
#'
#' @export export_tdf
#'
#' @examples
#'
#' ## Exporting a test data set. Creating a *data* data.frame with data on
#' ## 5 individuals.
#' d <- data.frame(
#'     aid = c("00101", "00102", "00103", "00104", "00105"),
#'     x0_sex = factor(c("Male", "Female", "Female", NA, "Male")),
#'     x0_age = c(45, 54, 33, 36, 66),
#'     x0_weight = c(78.5, 57.2, 55.2, 67.9, 84.2))
#'
#' ## Generate a *labels* data.frame from the data
#' l <- labels_from_data(d)
#' l
#'
#' ## Fill missing information to labels
#' l$unit <- c(NA, "Year", "kg")
#' l$description <- c("Sex", "Age", "Weight")
#'
#' ## Generate a *mapping* data.frame from data
#' m <- mapping_from_data(d)
#' m
#'
#' ## Create a simple grouping of all variables into a "general information"
#' ## group
#' g <- data.frame(
#'     group = c("ginfo", "ginfo", "ginfo"),
#'     label = c("x0_sex", "x0_age", "x0_weight"))
#'
#' ## Define a description for the group
#' gl <- data.frame(group = "ginfo", description = "General information")
#'
#' ## Now export all data to a temporary folder
#' path <- tempdir()
#'
#' ## Export the data specifying the name of the module, the version and other
#' ## information
#' export_tdf(name = "test_data", description = "Simple test data.",
#'     version = "1.0.0", date = date(), path = path, data = d,
#'     groups = g, group_labels = gl, labels = l, mapping = m)
export_tdf <- function(name = character(), description = character(),
                       version = character(), date = character(),
                       path = ".", data = data.frame(), groups = data.frame(),
                       group_labels = data.frame(),
                       labels = labels_from_data(data),
                       mapping = mapping_from_data(data), na = -89) {
    if (!length(name))
        stop("'name' has to be defined")
    if (!length(version))
        stop("'version' has to be defined")
    module_path <- file.path(path, name)
    if (!dir.exists(module_path)) dir.create(module_path)
    module_path <- file.path(module_path, version)
    if (!dir.exists(module_path)) dir.create(module_path)
    docs_path <- file.path(module_path, "docs")
    if (!dir.exists(docs_path)) dir.create(docs_path)
    module_path <- file.path(module_path, "data")
    if (!dir.exists(module_path)) dir.create(module_path)
    info_file <- file.path(module_path, "info.txt")
    if (file.exists(info_file))
        stop("A data set with the specified name and version does ",
             "already exist")
    fl <- file.path(docs_path, "docs.txt")
    file.create(fl)
    writeLines("filename\tname\tdescription",
               con = fl)
    ## Checking and exporting...
    if (nrow(data))
        .valid_data(data, stop = TRUE)
    else data <- .empty_data()
    if (nrow(groups))
        .valid_groups(groups, stop = TRUE)
    else groups <- .empty_groups()
    if (nrow(group_labels))
        .valid_group_labels(group_labels, stop = TRUE)
    else group_labels <- .empty_group_labels()
    dtypes <- vapply(data, function(z) class(z)[1L], character(1))
    if (nrow(labels)) {
        ## Check if columns min, max and missing are there...
        minmax <- .data_min_max(data)
        minmax <- minmax[rownames(minmax) != "aid", ]
        if (!any(colnames(labels) == "missing")) {
            labels$missing <- NA
            labels$missing[dtypes %in% c("numeric", "integer",
                                         "factor", "logical")] <- na
        }
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
    .valid_groups_group_labels(groups, group_labels, stop = TRUE)
    ## Actual exporting
    .info_skeleton(name = name, description = description,
                   version = version, date = date, path = module_path)
    .export_data(.format_data_export(data, na = na), path = module_path)
    .export_groups(groups, path = module_path)
    .export_group_labels(group_labels, path = module_path)
    .export_labels(labels, path = module_path)
    .export_labels_modules(labels, module = name, path = module_path)
    .export_mapping(mapping, path = module_path)
    ## Create a NEWS.md file.
    news_file <- file.path(path, name, "NEWS.md")
    if (file.exists(news_file)) {
        con <- file(news_file)
        open(con, open = "a")
        writeLines("\n", con = con)
    } else {
        file.create(news_file)
        con <- file(news_file)
        open(con, open = "w")
    }
    writeLines(paste0("# ", name, " version ", version), con = con)
    close(con)
    message("Data set was written to: ", module_path, "\n")
    invisible(module_path)
}

#' @export
#'
#' @rdname export_tdf
labels_from_data <- function(data, na = -89) {
    .valid_data(data, stop = TRUE)
    dtypes <- vapply(data, function(z) class(z)[1L], character(1))
    labels <- data.frame(label = names(dtypes),
                         type = .LABEL_DATA_TYPES[dtypes],
                         missing = NA)
    labels$missing[dtypes %in% c("numeric", "integer",
                                 "factor", "logical")] <- na
    mm <- .data_min_max(data)
    .fill_labels(cbind(labels, mm))[names(dtypes) != "aid", ]
}

#' @export
#'
#' @rdname export_tdf
mapping_from_data <- function(data) {
    .valid_data(data, stop = TRUE)
    dtypes <- vapply(data, function(z) class(z)[1L], character(1))
    idx <- which(dtypes == "factor")
    if (length(idx)) {
        do.call(rbind, lapply(idx, function (i) {
            lvls <- levels(data[, i])
            data.frame(label = names(dtypes)[i],
                       code = seq_along(lvls),
                       value = lvls)
        }))
    } else .empty_mapping()
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

.empty_group_labels <- function() {
    data.frame(group = character(), description = character())
}

.fill_labels <- function(labels = data.frame()) {
    cols <- c("label", "unit", "type", "min", "max", "missing", "description")
    for (col in cols[!cols %in% colnames(labels)]) {
        labels[[col]] <- rep("", nrow(labels))
    }
    add_cols <- setdiff(colnames(labels), cols)
    labels[, c(cols, add_cols)]
}

#' @importFrom utils packageVersion
.info_skeleton <- function(name = "", description = "", version = "",
                           date = "", path = ".") {
    out <- paste0("key\tdescription\n",
                  "name\t", name, "\n",
                  "description\t", description, "\n",
                  "version\t", version, "\n",
                  "date\t", date, "\n",
                  "export_date\t", date(), "\n",
                  "export_info\texported with tidyfr version ",
                  packageVersion("tidyfr"))
    writeLines(out, con = file.path(path, "info.txt"))
}

#' @importFrom utils write.table
.export_data <- function(path = ".", data = data.frame()) {
    write.table(data, sep = "\t", quote = FALSE, row.names = FALSE,
                file = file.path(path, "data.txt"), na = "")
}

.export_mapping <- function(path = ".", mapping = .empty_mapping()) {
    write.table(mapping, sep = "\t", quote = FALSE, na = "",
                row.names = FALSE, file = file.path(path, "mapping.txt"))
}

#' @param labels `data.frame` with columns that should be exported.
#'
#' @noRd
.export_labels <- function(path = ".", labels = data.frame()) {
    labels <- .fill_labels(labels)
    cn <- c("label", "unit", "type", "min", "max", "missing", "description")
    cn_add <- c("label", colnames(labels)[!colnames(labels) %in% cn])
    write.table(labels[, cn], sep = "\t", quote = FALSE, na = "",
                row.names = FALSE, file = file.path(path, "labels.txt"))
    if (length(cn_add) > 1L)
        write.table(labels[, cn_add], sep = "\t", quote = FALSE, na = "",
                    row.names = FALSE,
                    file = file.path(path, "labels_additional_info.txt"))
}

.export_labels_modules <- function(path = ".", labels = data.frame(),
                                   module = character()) {
    labels <- .fill_labels(labels)
    l <- data.frame(label = labels$label, module = rep(module, nrow(labels)))
    write.table(l, sep = "\t", quote = FALSE, na = "", row.names = FALSE,
                file = file.path(path, "labels_modules.txt"))
}


.export_groups <- function(path = ".", groups = .empty_groups()) {
    write.table(groups, sep = "\t", quote = FALSE, na = "",
                row.names = FALSE, file = file.path(path, "groups.txt"))
}

.export_group_labels <- function(path = ".",
                                 group_labels = .empty_group_labels()) {
    write.table(group_labels, sep = "\t", quote = FALSE, na = "",
                row.names = FALSE, file = file.path(path, "group_labels.txt"))
}
