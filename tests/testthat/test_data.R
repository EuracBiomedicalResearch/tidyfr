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

test_that(".format_float_import works", {
    vals <- c("34.1", "-89", "43.1")
    res <- .format_float_import(vals, -89)
    expect_true(is.numeric(res))
    expect_identical(res, c(34.1, NA_real_, 43.1))

    vals <- 1:4
    res <- .format_float_import(vals, -89)
    expect_true(is.numeric(res))
    expect_identical(res, c(1.0, 2.0, 3.0, 4.0))
})

test_that(".format_integer_import works", {
    vals <- c(1, -89.0, 3.0, 5)
    res <- .format_integer_import(vals, -89)
    expect_true(is.integer(res))
    expect_identical(res, c(1L, NA_integer_, 3L, 5L))
})

test_that(".format_character_import works", {
    vals <- c(1, 2, -89, 3)
    res <- .format_character_import(vals, -89)
    expect_true(is.character(res))
    expect_identical(res, c("1", "2", NA_character_, "3"))
})

test_that(".format_categorical_import works", {
    map <- data.frame(label = "l", code = 1:4, value = c("b", "c", "a", "d"))
    vals <- c(3, 1, 4, 2, 1, 3, -89, 3)
    res <- .format_categorical_import(vals, -89, "l", map)
    expect_true(is.factor(res))
    expect_equal(levels(res), c("b", "c", "a", "d"))
    expect_equal(res, factor(c("a", "b", "d", "c", "b", "a", NA, "a"),
                             levels = c("b", "c", "a", "d")))
})
