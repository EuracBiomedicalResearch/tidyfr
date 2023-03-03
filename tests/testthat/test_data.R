test_that("format import/export categorical works", {
    a <- factor(c("b", "a", "a", "d", "c", "b"))
    map <- data.frame(label = "a", value = c("a", "b", "c", "d"),
                      code = c(1, 2, 3, 4))
    res <- .format_categorical_export(a)
    expect_equal(res, c(2L, 1L, 1L, 4L, 3L, 2L))
    res_in <- .format_categorical_import(res, -89, "a", map)
    expect_equal(a, res_in)

    a <- factor(c("b", "a", "a", "d", "c", "b"), levels = c("d", "a", "b", "c"))
    map <- data.frame(label = "a", value = c("d", "a", "b", "c"),
                      code = c(1, 2, 3, 4))
    res <- .format_categorical_export(a)
    expect_equal(res, c(3L, 2L, 2L, 1L, 4L, 3L))
    res <- .format_categorical_import(res, -89, "a", map)
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

test_that(".format_date_import works", {
    vals <- c("2022-03-14", "2022-04-12", "-89", "2021-12-23")
    res <- .format_date_import(vals, na = -89)
    expect_true(is(res, "POSIXlt"))
    expect_true(res[4] < res[1])
    expect_true(res[1] < res[2])
    expect_true(is.na(res[3]))
})

test_that(".format_time_import works", {
    vals <- c("23:15", "missing", "23:15:12", "5:12:43")
    res <- .format_time_import(vals, na = "missing")
    expect_true(is(res, "POSIXlt"))
    expect_true(is.na(res[1]))
    expect_true(is.na(res[2]))
    expect_true(res[3] > res[4])
})

test_that(".format_datetime_import works", {
    vals <- c("2022-04-13T12:13:15", "2022-04-13T12:13:12", "other")
    res <- .format_datetime_import(vals, na = "other")
    expect_true(is(res, "POSIXlt"))
    expect_true(is.na(res[3]))
    expect_true(res[2] < res[1])
})

test_that(".format_boolean_import works", {
    vals <- c(1, 0, 1, 1, -1)
    res <- .format_boolean_import(vals, na = -1)
    expect_true(is.logical(res))
    expect_equal(res, c(TRUE, FALSE, TRUE, TRUE, NA))
})

test_that(".data_import works", {
    p1 <- system.file("txt", "db_example1", "1.0.0", "data", package = "tidyfr")
    res <- tidyfr:::.data_import(p1)
    res2 <- tidyfr:::.data_import(p1, aidAsRownames = FALSE)
    expect_equal(colnames(res2), c("aid", colnames(res)))
    expect_equal(res2$aid, rownames(res))
    expect_true(is.factor(res$x0_sex))
    expect_equal(levels(res$x0_sex), c("Male", "Female"))
    expect_true(is.numeric(res$x0_age))
    expect_true(is.integer(res$x0_ager))
    expect_equal(grep("^001", rownames(res)), seq_len(nrow(res)))

    p2 <- system.file("txt", "db_example2", "1.0.0", "data", package = "tidyfr")
    res <- .data_import(p2)

    p3 <- system.file("txt", "db_example2", "1.0.1", "data", package = "tidyfr")
    res <- .data_import(p3)
    expect_true(is.factor(res$x0_sex))
    expect_true(is.factor(res$x0_birthpc))
    expect_true(is.factor(res$x0_residpc))
    expect_true(is.character(res$x0_workf))
    expect_true(is.character(res$x0_note))
    expect_true(is.character(res$x0_notesaliva))
    expect_true(is.character(res$x0_noteint))
    expect_true(is.character(res$x0_noteself))
    expect_true(is.character(res$x0_notespiro))
    expect_true(is.numeric(res$x0_age))
    expect_true(is.integer(res$x0_ager))
    expect_true(is.integer(res$x0_birthm))
    expect_true(is.integer(res$x0_birthy))
    expect_true(is.integer(res$x0_examm))
    expect_true(is.integer(res$x0_examy))
    expect_true(is(res$x0_examd, "POSIXlt"))
    expect_true(is(res$x0_birthd, "POSIXlt"))
})
