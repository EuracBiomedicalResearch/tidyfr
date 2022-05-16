mat <- matrix(rnorm(60), ncol = 6, nrow = 10)
colnames(mat) <- paste0("001", 1:6)
rownames(mat) <- paste0("anlt", 1:10)
rowd <- data.frame(analyte = rownames(mat),
                   name = letters[1:10], add_col = 1:10)
rownames(rowd) <- rownames(mat)
se <- SummarizedExperiment(list(conc = mat, twice = mat * 2), rowData = rowd)

test_that(".data_from_SummarizedExperiment works", {
    res <- .data_from_SummarizedExperiment(se)
    expect_true(is.data.frame(res))
    expect_equal(colnames(res)[1L], "aid")
    expect_equal(nrow(res), ncol(se))

    res <- .data_from_SummarizedExperiment(se, label_prefix = "aa")
    expect_equal(unname(res$aa01), unname(assay(se, "conc")["anlt1", ]))
    expect_equal(unname(res$aa01a), unname(assay(se, "twice")["anlt1", ]))

    res <- .data_from_SummarizedExperiment(se, assayNames = "twice")
    expect_equal(unname(res$x0xx01), unname(assay(se, "twice")["anlt1", ]))
})

test_that(".groups_from_SummarizedExperiment works", {
    res <- .groups_from_SummarizedExperiment(se)
    expect_true(is.data.frame(res))
    expect_equal(colnames(res), c("group", "label"))

    res <- .groups_from_SummarizedExperiment(se, assayNames. = "twice",
                                             label_prefix = "aa")
    expect_true(is.data.frame(res))
    expect_equal(colnames(res), c("group", "label"))
    expect_true(all(res$group == "assay_twice"))
})

test_that(".labels_from_SummarizedExperiment works", {
    res <- .labels_from_SummarizedExperiment(se)

    expect_equal(nrow(res), 2 * nrow(se))
    expect_equal(colnames(res), c("label", "unit", "type", "min", "max",
                                  "missing", "description",
                                  colnames(rowData(se))))
})

test_that("chris_data,SummarizedExperiment works", {
    res <- chris_data(se, assayNames = assayNames(se))
    expect_true(ncol(res) == 21)
    expect_true(nrow(res) == 6)

    res <- chris_data(se, assayNames = "conc")
    expect_true(ncol(res) == 11)
    expect_true(nrow(res) == 6)
})

test_that("chris_labels,SummarizedExperiment works", {
    res <- chris_labels(se, assayNames = "conc")
    expect_true(nrow(res) == 10)
    expect_true(all(res$type == "float"))
    expect_equal(res$add_col, 1:10)
})

test_that("chris_groups,SummarizedExperiment works", {
    res <- chris_groups(se, assayNames = "conc")
    expect_true(nrow(res) == 10)
    expect_true(all(res$group == "assay_conc"))

    res <- chris_groups(se, assayNames = c("conc", "twice"))
    expect_true(nrow(res) == 20)
    expect_true(all(res$group %in% c("assay_conc", "assay_twice")))
})
