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
#'
#' @noRd
.format_data_import <- function() {

}

.format_categorical_export <- function(x) {
    as.integer(x)
}

.format_categorical_import <- function(x, levels) {
    factor(levels[x], levels = levels)
}

.format_date_import <- function(x) {
    stop("Date support not yet implemented")
}

.format_date_export <- function(x) {
    stop("Date support not yet implemented")
}
