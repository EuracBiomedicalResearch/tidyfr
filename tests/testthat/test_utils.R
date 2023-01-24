test_that(".bump_version works", {
    res <- .bump_version(3)
    expect_equal(res, "3.0.0.1")
    res <- .bump_version("4.2.4")
    expect_equal(res, "4.2.4.1")
    res <- .bump_version("4.2.2.2")
    expect_equal(res, "4.2.2.3")
})

test_that("format_aid works", {
    res <- format_aid(c(1, 12, 2), length = 5)
    expect_equal(res, c("00001", "00012", "00002"))

    res <- format_aid(c("a", "b", "c"))
    expect_equal(res, c("a", "b", "c"))
})
