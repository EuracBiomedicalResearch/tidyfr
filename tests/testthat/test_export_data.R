test_that(".info_skeleton works", {
    fl <- tempdir()
    .info_skeleton(name = "test", version = 200, path = fl)
    res <- read.table(file.path(fl, "info.txt"), sep = "\t", header = TRUE)
    expect_equal(colnames(res), c("key", "description"))
    expect_equal(unlist(res[1, ], use.names = FALSE), c("name", "test"))
    expect_equal(unlist(res[2, ], use.names = FALSE), c("description", ""))
    expect_equal(unlist(res[3, ], use.names = FALSE), c("version", "200"))
    expect_equal(unlist(res[4, ], use.names = FALSE), c("date", ""))
})

test_that(".empty_mapping works", {
    res <- .empty_mapping()
    expect_true(is.data.frame(res))
    expect_equal(colnames(res), c("label", "code", "value"))
})

test_that(".export_mapping works", {
    fl <- tempdir()
    .export_mapping(fl)
    res <- read.table(file.path(fl, "mapping.txt"), sep = "\t", header = TRUE)
    expect_equal(colnames(res), c("label", "code", "value"))
    expect_true(nrow(res) == 0)

    df <- data.frame(label = c("a", "a"), code = 1:2)
    df$value <- c("A", "B")
    .export_mapping(fl, mapping = df)
    res <- read.table(file.path(fl, "mapping.txt"), sep = "\t", header = TRUE)
    expect_equal(res, df)
})

test_that(".fill_labels works", {
    res <- .fill_labels()
    expect_true(is.data.frame(res))
    cols <- c("label", "unit", "type", "min", "max", "missing", "description")
    expect_equal(colnames(res), cols)

    res <- .fill_labels(data.frame(type = "a"))
    expect_true(is.data.frame(res))
    expect_equal(colnames(res), cols)
    expect_equal(res$type, "a")

    res <- .fill_labels(data.frame(other = "a"))
    expect_true(is.data.frame(res))
    expect_equal(colnames(res), c(cols, "other"))
    expect_equal(res$other, "a")
})

test_that(".export_labels works", {
    fl <- tempdir()
    .export_labels(path = fl)
    res <- read.table(file.path(fl, "labels.txt"), sep = "\t", header = TRUE)
    expect_true(nrow(res) == 0)
    expect_equal(colnames(res), c("label", "unit", "type", "min", "max",
                                  "missing", "description"))

    df <- data.frame(label = 1:10, type = "numeric")
    .export_labels(path = fl, labels = df)
    res <- read.table(file.path(fl, "labels.txt"), sep = "\t", header = TRUE)
    expect_true(nrow(res) == 10)
    expect_equal(colnames(res), c("label", "unit", "type", "min", "max",
                                  "missing", "description"))
    expect_equal(res$label, 1:10)
    expect_true(all(res$type == "numeric"))
})

test_that(".empty_groups works", {
    res <- .empty_groups()
    expect_true(is.data.frame(res))
    expect_equal(colnames(res), c("group", "label"))
})

test_that(".export_groups works", {
    fl <- tempdir()
    .export_groups(fl)
    res <- read.table(file.path(fl, "groups.txt"), sep = "\t", header = TRUE)
    expect_equal(colnames(res), c("group", "label"))
    expect_true(nrow(res) == 0)

    df <- data.frame(group = "a", label = 1:4)
    .export_groups(fl, df)
    res <- read.table(file.path(fl, "groups.txt"), sep = "\t", header = TRUE)
    expect_equal(colnames(res), c("group", "label"))
    expect_true(all(res$group == "a"))
    expect_equal(res$label, 1:4)
})

test_that(".empty_grp_labels works", {
    res <- .empty_grp_labels()
    expect_true(is.data.frame(res))
    expect_equal(colnames(res), c("group", "description"))
})

test_that(".export_grp_labels works", {
    fl <- tempdir()
    .export_grp_labels(fl)
    res <- read.table(file.path(fl, "grp_labels.txt"),
                      sep = "\t", header = TRUE)
    expect_equal(colnames(res), c("group", "description"))
    expect_true(nrow(res) == 0)

    df <- data.frame(group = "a", description = 1:10)
    .export_grp_labels(fl, df)
    res <- read.table(file.path(fl, "grp_labels.txt"),
                      sep = "\t", header = TRUE)
    expect_equal(colnames(res), c("group", "description"))
    expect_true(all(res$group == "a"))
    expect_equal(res$description, 1:10)
})

test_that(".replace_na_data works", {
    res <- .replace_na_data(data.frame(aid = character()))
    expect_equal(res, data.frame(aid = character()))

    data <- data.frame(aid = 1:4, a = c(3, 5, NA, 5), b = c(4, NA, NA, 4))
    res <- .replace_na_data(data)
    expect_equal(res$a, c(3, 5, -89, 5))
    expect_equal(res$b, c(4, -89, -89, 4))
})

test_that".data_min_max works", {
    data <- data.frame(aid = 1:4, a = c(3, 5, NA, 5), b = c(4, NA, NA, 4),
                       c = FALSE, d = "other")
    res <- .data_min_max(data)
    expect_true(is.data.frame(res))
    expect_equal(rownames(res), colnames(data))
    expect_equal(res$min, c(1, 3, 4, NA, NA))
    expect_equal(res$max, c(4, 5, 4, NA, NA))
})

test_that("export_ctff works", {
    pth <- "/Users/jo/Desktop/"
    expect_error(export_ctff(), "'name'")
    expect_error(export_ctff(name = "a"), "'version'")
    res <- export_ctff(name = "a", version = "0.0.1", path = pth)
    expect_error(export_ctff(name = "a", version = "0.0.1", path = pth),
                 "does already exist")
    expect_length(res, 1)
    tmp <- .info(res)
    expect_equal(tmp$description[tmp$key == "name"], "a")
    expect_equal(tmp$description[tmp$key == "version"], "0.0.1")
    tmp <- .data(res)
    expect_true(is.data.frame(tmp))
    expect_equal(colnames(tmp), "aid")
    expect_true(.check_dataset_content(res))

    ## Now with real data.
    data <- data.frame(aid = as.character(1:5),
                       a = c(12, 34, NA, 45, 12),
                       b = c(TRUE, FALSE, TRUE, FALSE, FALSE),
                       c = "other",
                       f = factor(c("a", "b", "b", "a", "d"),
                                  levels = c("b", "a", "d")),
                       d = c(4, 2, NA, 1, 1))
    res <- export_ctff(data = data, name = "a", version = "0.0.2", path = pth)

})
