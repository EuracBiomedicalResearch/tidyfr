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

#' Function to import and format the data of a module.
#'
#' @param path `character(1)` with the module path
#'
#' @noRd
.data_import <- function(path, ...) {
    d <- .data(path)
    l <- .labels(path)
    m <- .mapping(path)
    cn <- colnames(d)
    for (label in colnames(d)) {
        if (label == "aid") {
            d[["aid"]] <- .format_aid(d[["aid"]], 10)
            next
        }
        na <- l[l$label == label, "missing"]
        fun <- .FORMAT_IMPORT[[l[l$label == label, "type"]]]
        if (!is.null(fun))
            d[[label]] <- fun(d[, label], na = na, label = label, mapping = m)
        else warning("No formatter for ", l[l$label == label, "type"])
    }
    d
}

.format_aid <- function(x, length = 10) {
    sprintf(paste0("%0", length, "d"), x)
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
    map <- map[map$code != na, , drop = FALSE]
    factor(map$value[match(x, map$code)], levels = map$value)
}

.format_character_import <- function(x, na, label, mapping, ...) {
    x[x == na] <- NA_character_
    as.character(x)
}

.format_date_import <- function(x, na, format = "%Y-%m-%d", ...) {
    x[x == na] <- NA
    strptime(x, format = format)
}

.format_time_import <- function(x, na, format = "%H:%M:%S", ...) {
    x[x == na] <- NA
    strptime(x, format = format)
}

.format_datetime_import <- function(x, na, format = "%Y-%m-%dT%H:%M:%S",...) {
    x[x == na] <- NA
    strptime(x, format = format)
}

.format_boolean_import <- function(x, na, ...) {
    x[x == na] <- NA
    as.logical(x)
}

.FORMAT_IMPORT <- list(
    integer = .format_integer_import,
    float = .format_float_import,
    categorical= .format_categorical_import,
    character = .format_character_import,
    date = .format_date_import,
    time = .format_time_import,
    datetime = .format_datetime_import,
    boolean = .format_boolean_import)

.format_categorical_export <- function(x) {
    as.integer(x)
}

.format_date_export <- function(x) {
    stop("Date support not yet implemented")
}
