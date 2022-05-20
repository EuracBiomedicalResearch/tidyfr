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
    expect_equal(res$group, c(rep("assay_conc", 10),
                              rep("assay_twice", 10),
                              c("analyte_x0xx01", "analyte_x0xx01",
                                "analyte_x0xx02", "analyte_x0xx02",
                                "analyte_x0xx03", "analyte_x0xx03",
                                "analyte_x0xx04", "analyte_x0xx04",
                                "analyte_x0xx05", "analyte_x0xx05",
                                "analyte_x0xx06", "analyte_x0xx06",
                                "analyte_x0xx07", "analyte_x0xx07",
                                "analyte_x0xx08", "analyte_x0xx08",
                                "analyte_x0xx09", "analyte_x0xx09",
                                "analyte_x0xx10", "analyte_x0xx10")))
    expect_equal(res$label,
                 c("x0xx01", "x0xx02", "x0xx03", "x0xx04", "x0xx05",
                   "x0xx06", "x0xx07", "x0xx08", "x0xx09", "x0xx10",
                   "x0xx01a", "x0xx02a", "x0xx03a", "x0xx04a", "x0xx05a",
                   "x0xx06a", "x0xx07a", "x0xx08a", "x0xx09a", "x0xx10a",
                   "x0xx01", "x0xx01a", "x0xx02", "x0xx02a",
                   "x0xx03", "x0xx03a", "x0xx04", "x0xx04a",
                   "x0xx05", "x0xx05a", "x0xx06", "x0xx06a",
                   "x0xx07", "x0xx07a", "x0xx08", "x0xx08a",
                   "x0xx09", "x0xx09a", "x0xx10", "x0xx10a"))

    res <- .groups_from_SummarizedExperiment(se, assayNames. = "twice",
                                             label_prefix = "aa")
    expect_true(is.data.frame(res))
    expect_equal(colnames(res), c("group", "label"))
    expect_equal(unique(res$group),
                 c("assay_twice", "analyte_aa01", "analyte_aa02",
                   "analyte_aa03", "analyte_aa04", "analyte_aa05",
                   "analyte_aa06", "analyte_aa07", "analyte_aa08",
                   "analyte_aa09", "analyte_aa10"))
})

test_that(".labels_from_SummarizedExperiment works", {
    res <- .labels_from_SummarizedExperiment(se)

    expect_equal(nrow(res), 2 * nrow(se))
    expect_equal(colnames(res), c("label", "unit", "type", "min", "max",
                                  "missing", "description",
                                  colnames(rowData(se))))
})

test_that("data,SummarizedExperiment works", {
    res <- data(se, assayNames = assayNames(se))
    expect_true(ncol(res) == 21)
    expect_true(nrow(res) == 6)

    res <- data(se, assayNames = "conc")
    expect_true(ncol(res) == 11)
    expect_true(nrow(res) == 6)
})

test_that("labels,SummarizedExperiment works", {
    res <- labels(se, assayNames = "conc")
    expect_true(nrow(res) == 10)
    expect_true(all(res$type == "float"))
    expect_equal(res$add_col, 1:10)
})

test_that("groups,SummarizedExperiment works", {
    res <- groups(se, assayNames = "conc")
    expect_true(nrow(res) == 20)
    expect_true(all(res$group[1:10] == "assay_conc"))

    res <- groups(se, assayNames = c("conc", "twice"))
    expect_true(nrow(res) == 40)
    expect_true(all(res$group[1:20] %in% c("assay_conc", "assay_twice")))
})
