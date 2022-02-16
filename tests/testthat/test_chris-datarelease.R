test_that(".valid_chris_release works", {
    path <- tempdir()
    expect_false(.valid_chris_release(path))

    path <- system.file(package = "chrisr")
    expect_false(.valid_chris_release(path))

    path <- system.file("txt", package = "chrisr")
    expect_true(.valid_chris_release(path))
})

test_that(".valid_chris_data_path works", {
    path <- tempdir()
    expect_false(.valid_chris_data_path(path))

    path <- system.file("txt", package = "chrisr")
    expect_false(.valid_chris_data_path(path))

    path <- system.file(package = "chrisr")
    expect_true(.valid_chris_data_path(path))
})

test_that("setChrisDataPath works", {
    expect_error(setChrisDataPath(), "not contain")

    setChrisDataPath(system.file(package = "chrisr"))

    expect_error(setChrisDataPath(system.file("txt", package = "chrisr")),
                 "not contain")

    expect_error(setChrisDataPath(tempdir()), "not contain")
})

test_that("chrisDataPath works", {
    res <- chrisDataPath()
    expect_equal(res, Sys.getenv("CHRIS_DATA_PATH"))

    setChrisDataPath(system.file(package = "chrisr"))
    expect_equal(chrisDataPath(), system.file(package = "chrisr"))
})

test_that("chrisDataReleases works", {
    expect_true(length(chrisDataReleases(tempdir())) == 0)

    expect_warning(res <- chrisDataReleases(system.file(package = "chrisr")),
                   "help")
    expect_equal(res, "txt")

    expect_warning(res <- chrisDataReleases(
                       system.file("txt", package = "chrisr")), "data release")
    expect_equal(res, character())
})

test_that("chrisDataRelease works", {
    expect_equal(chrisDataRelease(), Sys.getenv("CHRIS_DATA_RELEASE"))

    Sys.setenv(CHRIS_DATA_RELEASE = 123)
    res <- chrisDataRelease()
    expect_equal(res, "123")
})

test_that("setChrisDataRelease works", {
    expect_error(setChrisDataRelease("a"), "not a valid")
})
