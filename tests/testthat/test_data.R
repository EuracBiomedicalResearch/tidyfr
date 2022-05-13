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
})
