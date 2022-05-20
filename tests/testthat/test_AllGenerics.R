test_that("data dispatches to utils::data", {
    expect_equal(data(iris), "iris")
    expect_true(is.data.frame(iris))
})
