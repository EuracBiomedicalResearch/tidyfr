ex1 <- system.file("txt", "db_example1", package = "chrisr")

test_that(".check_dataset_content works", {
    dr <- system.file("txt", "db_example1", package = "chrisr")
    expect_true(.check_dataset_content(dr))

    dr <- system.file("txt", package = "chrisr")
    expect_error(.check_dataset_content(dr), "missing one")
    expect_false(.check_dataset_content(dr, TRUE))
})

test_that(".data, .valid_data works", {
    data <- .data(ex1)
    expect_true(.valid_data(data))
    data$aid <- NULL
    expect_error(.valid_data(data), "lacks required")
})

test_that(".groups, .valid_groups works", {
    groups <- .groups(ex1)
    expect_true(.valid_groups(groups))
    groups$other <- "d"
    expect_error(.valid_groups(groups), "two columns")
    groups$other <- NULL
    colnames(groups) <- c("a", "b")
    expect_error(.valid_groups(groups), "required columns")
})

test_that(".grp_labels, .valid_grp_labels works", {
    grpl <- .grp_labels(ex1)
    expect_true(.valid_grp_labels(grpl))
    grpl$other <- "d"
    expect_error(.valid_grp_labels(grpl), "two columns")
    grpl$other <- NULL
    colnames(grpl) <- c("a", "b")
    expect_error(.valid_grp_labels(grpl), "required columns")
})

test_that(".info, .valid_info works", {
    info <- .info(ex1)
    expect_true(ncol(info) == 2)
    expect_true(.valid_info(info))
    info$other <- "b"
    expect_error(.valid_info(info), "two columns")
    info$other <- NULL
    colnames(info) <- c("a", "b")
    expect_error(.valid_info(info), "to be called")
    colnames(info) <- c("key", "description")
    info <- info[1:3, ]
    expect_error(.valid_info(info), "required key")
})

test_that(".labels, .valid_labels works", {
    l <- .labels(ex1)
    expect_true(.valid_labels(l))
    l$type <- NULL
    expect_error(.valid_labels(l), "required columns")
    l <- .labels(ex1)
    l[2, "label"] <- NA
    expect_error(.valid_labels(l), "missing or empty")
    l[2, "label"] <- ""
    expect_error(.valid_labels(l), "missing or empty")
    l <- .labels(ex1)
    l[3, "type"] <- "unknown"
    expect_error(.valid_labels(l), "unsupported")
})

test_that(".mapping, .valid_mapping works", {
    m <- .mapping(ex1)
    expect_true(.valid_mapping(m))
    m$value <- NULL
    expect_error(.valid_mapping(m), "required columns")
})
