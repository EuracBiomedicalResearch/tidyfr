ex1 <- system.file("txt", "db_example1", "1.0.0", "data", package = "tidyfr")
ex2 <- system.file("txt", "db_example2", "1.0.0", "data", package = "tidyfr")
ex3 <- system.file("txt", "db_example2", "1.0.1", "data", package = "tidyfr")

test_that(".valid_data_directory works", {
    expect_match(.valid_data_directory(tempdir()), "missing")
    expect_error(.valid_data_directory(tempdir(), stop = TRUE), "missing")

    expect_true(.valid_data_directory(ex1))
    expect_true(.valid_data_directory(ex3))
})

test_that("DataModule validator works", {
    m <- new("DataModule")
    expect_true(validObject(m))
    m@path <- "not there"
    expect_error(validObject(m), "not exist")
})

test_that("moduleName works", {
    m <- new("DataModule", name = "the name")
    expect_equal(moduleName(m), "the name")
})

test_that("moduleVersion works", {
    m <- new("DataModule", version = "1.0.0")
    expect_equal(moduleVersion(m), "1.0.0")
})

test_that("moduleDescription works", {
    m <- new("DataModule", description = "something")
    expect_equal(moduleDescription(m), "something")
})

test_that("moduleDate works", {
    m <- new("DataModule", date = "2022-05")
    expect_equal(moduleDate(m), "2022-05")
})

test_that("modulePath works", {
    m <- new("DataModule", path = tempdir())
    expect_equal(modulePath(m), tempdir())
})

test_that(".data, .valid_data works", {
    data <- .data(ex1)
    expect_true(length(.valid_data(data)) == 0)
    data$aid <- NULL
    expect_error(.valid_data(data), "lacks required")
    expect_true(length(.valid_data(data, stop = FALSE)) == 1)

    data <- data.frame(aid = 1:3, b = 3, c = 4L, d = "a", e = factor(1:3))
    expect_length(.valid_data(data, stop = FALSE), 0)
})

test_that(".groups, .valid_groups works", {
    groups <- .groups(ex1)
    expect_true(length(.valid_groups(groups)) == 0)
    groups$other <- "d"
    expect_error(.valid_groups(groups), "two columns")
    expect_true(length(.valid_groups(groups, stop = FALSE)) > 0)
    groups$other <- NULL
    colnames(groups) <- c("a", "b")
    expect_error(.valid_groups(groups), "columns named")
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
    expect_error(.valid_grp_labels(grpl), "columns named")
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
    expect_error(.valid_mapping(m), "3 columns")
    m$other <- 3
    expect_error(.valid_mapping(m), "columns named")
})

test_that(".valid_data_mapping_category_codes works", {
    data <- data.frame(aid = 1:10, a = 1:10,
                       b = c(1, 2, 3, 4, 1, 2, 3, 4, 5, 6))
    mapping <- data.frame(label = "b", code = 1:6,
                          value = c("a", "b", "c", "d", "e", "f"))
    expect_length(.valid_data_mapping_category_codes(data, mapping), 0)
    data$b[5] <- 10
    expect_match(
        .valid_data_mapping_category_codes(data, mapping, stop = FALSE),
        "values without encoding")

    mapping <- .empty_mapping()
    expect_length(.valid_data_mapping_category_codes(data, mapping), 0)
    data <- .empty_data()
    expect_length(.valid_data_mapping_category_codes(data, mapping), 0)
})

test_that(".valid_labels_mapping_categories works", {
    labels <- .fill_labels()
    mapping <- .empty_mapping()
    expect_length(.valid_labels_mapping_categories(labels, mapping), 0)
    labels <- .fill_labels(
        data.frame(type = c("numeric", "categorical", "categorical")))
    expect_error(.valid_labels_mapping_categories(labels, mapping), "encoding")
    labels$label <- c("a", "b", "c")
    mapping <- data.frame(label = c("b", "b"), code = 1:2,
                          value = c("BB", "BA"))
    expect_match(.valid_labels_mapping_categories(labels, mapping, FALSE),
                 "categorical variables")
    mapping <- data.frame(label = c("c", "c", "b", "b", "d"), code = 1:5,
                          value = c("CA", "CB", "BB", "BA", "Z"))
    expect_length(.valid_labels_mapping_categories(labels, mapping, FALSE), 0)
})

test_that(".valid_data_labels works", {
    labels <- .fill_labels()
    data <- data.frame(aid = character())
    expect_length(.valid_data_labels(data, labels), 0)

    data <- data.frame(aid = 1:3, a = 1, b = 2, c = 3)
    expect_error(.valid_data_labels(data, labels), "missing labels")
    labels <- .fill_labels(data.frame(label = c("a", "b", "c")))
    expect_length(.valid_data_labels(data, labels, stop = FALSE), 0)
})

test_that(".valid_labels_data_types works", {
    labels <- .fill_labels()
    expect_length(.valid_labels_data_types(labels), 0)

    labels <- .fill_labels(data.frame(type = c("float", "character", "other")))
    expect_error(.valid_labels_data_types(labels), "unsupported data")

    expect_length(.valid_labels_data_types(labels[1:2, ], stop = FALSE), 0)
})

test_that(".valid_data_groups works", {
    data <- data.frame(aid = 1:3, b = 1, c = 2, d = 3)
    groups <- .empty_groups()
    expect_length(.valid_data_groups(data, groups), 0)

    groups <- data.frame(group = "a", label = c("b", "c", "d", "e"))
    expect_error(.valid_data_groups(data, groups), "labels that are")

    expect_length(.valid_data_groups(data, groups[1:3, ]), 0)
})

test_that(".valid_groups_grp_labels works", {
    groups <- .empty_groups()
    grp_labels <- .empty_grp_labels()
    expect_length(.valid_groups_grp_labels(groups, grp_labels), 0)

    groups <- data.frame(group = c("a", "a", "b", "d"), label = 1:4)
    grp_labels <- data.frame(group = c("a", "b"), description = 1:2)
    expect_error(.valid_groups_grp_labels(groups, grp_labels), "missing")

    expect_length(.valid_groups_grp_labels(groups[1:3, ], grp_labels), 0)
})