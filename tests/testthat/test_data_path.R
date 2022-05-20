test_that("data_path works", {
    if (Sys.getenv("DATA_PATH") == "") {
        expect_warning(res <- data_path(), "globally")
    } else res <- data_path()
    expect_equal(res, Sys.getenv("DATA_PATH"))
})

test_that("list_data_modules works", {
    res <- list_data_modules(tempdir())

    expect_error(list_data_modules("not"), "not")

    res <- list_data_modules(system.file("txt", package = "tidyfr"))
    expect_true(is.data.frame(res))
    expect_true(nrow(res) == 3)
})
