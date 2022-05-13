
#' Format the data:
#' - convert to correct data types.
#' -
#'
#' @noRd
.format_data <- function(x, labels, categories) {
}

#'
.format_data_export <- function() {
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
