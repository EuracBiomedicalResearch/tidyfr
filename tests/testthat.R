library(testthat)

library(tidyfr)
expect_warning(res <- list_data_modules(tempdir()), "No data")
expect_true(is.null(res))

test_check("tidyfr")
