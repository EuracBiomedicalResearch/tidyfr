test_that("format import/export categorical works", {
    a <- factor(c("b", "a", "a", "d", "c", "b"))
    res <- .format_categorical_export(a)
    expect_equal(res, c(2L, 1L, 1L, 4L, 3L, 2L))
    res <- .format_categorical_import(res, levels(a))
    expect_equal(a, res)

    a <- factor(c("b", "a", "a", "d", "c", "b"), levels = c("d", "a", "b", "c"))
    res <- .format_categorical_export(a)
    expect_equal(res, c(3L, 2L, 2L, 1L, 4L, 3L))
    res <- .format_categorical_import(res, levels(a))
    expect_equal(a, res)

    a <- factor(c("a", NA, "b", "a"))
    res <- .format_categorical_export(a)
    expect_equal(res, c(1L, NA_integer_, 2L, 1L))
})

test_that(".format_data_export works", {
    data <- .empty_data()
    res <- .format_data_export(data)
    expect_equal(res, data)
    data <- data.frame(aid = 1:4, a = 1, b = 2, c = "D")
    res <- .format_data_export(data)
    expect_equal(res, data)

    data$f <- factor(c(1, 2, 2, 1))
    data$n <- c(3, NA, 4, NA)
    res <- .format_data_export(data)
    expect_equal(res$f, c(1L, 2L, 2L, 1L))
    expect_equal(res$n, c(3, -89, 4, -89))
})
