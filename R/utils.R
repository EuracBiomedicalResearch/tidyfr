.bump_version <- function(x, length = 4) {
    vals <- as.integer(unlist(strsplit(as.character(x), ".", fixed = TRUE)))
    while (length(vals) < length)
        vals <- c(vals, 0L)
    vals[length(vals)] <- vals[length(vals)] + 1L
    paste0(vals, collapse = ".")
}

#' Move up for x folders up. Works at present only for "/" and does also not
#' do any checks.
#'
#' @noRd
.path_up <- function(p, x = integer()) {
    p <- unlist(strsplit(p, "/", fixed = TRUE))
    paste0(p[seq_len(length(p) - x)], collapse = "/")
}

#' @title Format AIDs adding leading 0s
#'
#' @description
#'
#' The `format_aid` function formats (numeric) AID IDs into character strings
#' of a predefined length (parameter `length`) adding leading `"0"`s to the
#' provided ID (`x`).
#'
#' @param x `numeric` with the IDs to format.
#'
#' @param length `integer(1)` defining the length (in characters) of the AID
#'     format.
#'
#' @return `character` with the formated AID.
#'
#' @author Johannes Rainer
#'
#' @export
#'
#' @examples
#'
#' format_aid(c(1, 2, 3))
#'
#' ## IDs provided as `character` are not formatted.
#' format_aid("hello")
format_aid <- function(x, length = 10) {
    if (is.numeric(x))
        .format_aid(x, length = length)
    else x
}
