ex1 <- system.file("txt", "db_example1", package = "chrisr")

test_that(".check_dataset_content works", {
    dr <- system.file("txt", "db_example1", package = "chrisr")
    expect_true(.check_dataset_content(dr))

    dr <- system.file("txt", package = "chrisr")
    expect_error(.check_dataset_content(dr, stop = TRUE), "missing one")
    expect_false(.check_dataset_content(dr, stop = FALSE))
})

test_that(".data, .valid_data works", {
    data <- .data(ex1)
    expect_true(length(.valid_data(data)) == 0)
    data$aid <- NULL
    expect_error(.valid_data(data), "lacks required")
    expect_true(length(.valid_data(data, stop = FALSE)) == 1)
})

test_that(".groups, .valid_groups works", {
    groups <- .groups(ex1)
    expect_true(length(.valid_groups(groups)) == 0)
    groups$other <- "d"
    expect_error(.valid_groups(groups), "two columns")
    expect_true(length(.valid_groups(groups, stop = FALSE)) > 0)
    groups$other <- NULL
    colnames(groups) <- c("a", "b")
    expect_error(.valid_groups(groups), "required columns")
    expect_true(length(.valid_groups(groups, stop = FALSE)) > 0)
})

test_that(".grp_labels, .valid_grp_labels works", {
    grpl <- .grp_labels(ex1)
    expect_true(length(.valid_grp_labels(grpl)) == 0)
    grpl$other <- "d"
    expect_error(.valid_grp_labels(grpl), "two columns")
    expect_true(length(.valid_grp_labels(grpl, stop = FALSE)) > 0)
    grpl$other <- NULL
    colnames(grpl) <- c("a", "b")
    expect_error(.valid_grp_labels(grpl), "required columns")
    expect_true(length(.valid_grp_labels(grpl, stop = FALSE)) > 0)
})

test_that(".info, .valid_info works", {
    info <- .info(ex1)
    expect_true(ncol(info) == 2)
    expect_true(length(.valid_info(info)) == 0)
    info$other <- "b"
    expect_error(.valid_info(info), "two columns")
    expect_true(length(.valid_info(info, stop = FALSE)) > 0)
    info$other <- NULL
    colnames(info) <- c("a", "b")
    expect_error(.valid_info(info), "to be called")
    expect_true(length(.valid_info(info, stop = FALSE)) > 0)
    colnames(info) <- c("key", "description")
    info <- info[1:3, ]
    expect_error(.valid_info(info), "required key")
    expect_true(length(.valid_info(info, stop = FALSE)) > 0)
})

test_that(".labels, .valid_labels works", {
    l <- .labels(ex1)
    expect_true(length(.valid_labels(l)) == 0)
    l$type <- NULL
    expect_error(.valid_labels(l), "required columns")
    expect_true(length(.valid_labels(l, stop = FALSE)) > 0)
    l <- .labels(ex1)
    l[2, "label"] <- NA
    expect_error(.valid_labels(l), "missing or empty")
    expect_true(length(.valid_labels(l, stop = FALSE)) > 0)
    l[2, "label"] <- ""
    expect_error(.valid_labels(l), "missing or empty")
    expect_true(length(.valid_labels(l, stop = FALSE)) > 0)
    l <- .labels(ex1)
    l[3, "type"] <- "unknown"
    expect_error(.valid_labels(l), "unsupported")
    expect_true(length(.valid_labels(l, stop = FALSE)) > 0)
})

test_that(".mapping, .valid_mapping works", {
    m <- .mapping(ex1)
    expect_true(length(.valid_mapping(m)) == 0)
    m$value <- NULL
    expect_error(.valid_mapping(m), "required columns")
})
