#' @noRd
.format_data_export <- function(data, na = -89) {
    .valid_data(data, stop = TRUE)
    ## categorical/factor
    dtypes <- vapply(data, function(z) class(z)[1L], character(1))
    idx <- which(dtypes == "factor")
    for (i in idx)
        data[, i] <- .format_categorical_export(data[, i])
    data <- .replace_na_data(data, na = -89)
    ## date?
    idx <- which(dtypes %in% c("Date"))
    for (i in idx)
        data[, i] <- .format_date_export(data[, i])
    data
}

#' - categorical variables.
#' - date/time variables.
#' @param path `character(1)` with the module path
#'
#' @noRd
.data_import <- function(path) {
    d <- .data(path)
    l <- .labels(path)
    m <- .mapping(path)
    ## loop over data columns, get an importer function from labels.
    cn <- colnames(d)
    for (label in colnames(d)) {
        if (label == "aid") next
        na <- l[l$label == label, "missing"]
        fun <- .FORMAT_IMPORT[[l[l$label == label, "type"]]]
        if (!is.null(fun))
            d[, label] <- fun(d[, label], na = na, label = label, mapping = m)
        else warning("No formatter for ", l[l$label == label, "type"])
    }
    d
}

.format_float_import <- function(x, na, ...) {
    x[x == na] <- NA
    as.numeric(x)
}

.format_integer_import <- function(x, na, ...) {
    x[x == na] <- NA
    as.integer(x)
}

.format_categorical_import <- function(x, na, label, mapping, ...) {
    map <- mapping[mapping$label == label, ]
    x[x == na] <- NA
    factor(map$value[match(x, map$code)], levels = map$value)
}

.format_character_import <- function(x, na, label, mapping, ...) {
    x[x == na] <- NA_character_
    as.character(x)
}

.FORMAT_IMPORT <- list(
    integer = .format_integer_import,
    float = .format_float_import,
    categorical= .format_categorical_import,
    character = .format_character_import)

.format_categorical_export <- function(x) {
    as.integer(x)
}

## .format_categorical_import <- function(x, levels) {
##     factor(levels[x], levels = levels)
## }

.format_date_import <- function(x) {
    stop("Date support not yet implemented")
}

.format_date_export <- function(x) {
    stop("Date support not yet implemented")
}
