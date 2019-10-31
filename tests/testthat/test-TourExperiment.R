context("TourExperiment class and constructor")

suppressPackageStartupMessages(library(SingleCellExperiment))

olive <- tourr::olive
rownames(olive) <- as.character(1:nrow(olive))

test_that("constructor works", {
  se_olive <- TourExperiment(olive, palmitic:eicosenoic)
  expect_s4_class(se_olive, "TourExperiment")
  expect_true(is(se_olive, "SingleCellExperiment"))
  expect_identical(assayNames(se_olive), "view")
  
  # no selection puts everything into rowData
  se_olive <- TourExperiment(olive)
  expect_s4_class(se_olive, "TourExperiment")
  expect_true(is(se_olive, "SingleCellExperiment"))
  expect_equal(dim(assay(se_olive)), c(0, nrow(olive)))
  expect_identical(
    as.data.frame(colData(se_olive)), olive 
  )
  
  # selecting throws an error if non-homegenous types
  expect_error(TourExperiment(olive,  1:ncol(olive)))
  
  # matrix constructor 
  sphere <- generate_sphere(1000, 10, 0, 1)
  se_sphere <- TourExperiment(sphere)
  expect_s4_class(se_sphere, "TourExperiment")
  expect_true(is(se_sphere, "SingleCellExperiment"))
  expect_identical(assay(se_sphere), sphere)
  
  # SummarizedExperiment
  se <- SummarizedExperiment::SummarizedExperiment(assays = list(view = sphere))
  expect_identical(TourExperiment(se), se_sphere)
  
  # sce 
  se <- SingleCellExperiment(assays = list(view = sphere))
  expect_identical(TourExperiment(se), se_sphere)
})