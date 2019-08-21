context("SightSE class and constructor")

suppressPackageStartupMessages(library(SingleCellExperiment))

olive <- tourr::olive
rownames(olive) <- as.character(1:nrow(olive))

test_that("constructor works", {
  se_olive <- as_sightse(olive, 
                         traveller = tourr::grand_tour(),
                         palmitic:eicosenoic
                         )
  expect_s4_class(se_olive, "SightSE")
  expect_true(is(se_olive, "SingleCellExperiment"))
  expect_true(is(se_olive@traveller, "tour_path"))
  expect_identical(assayNames(se_olive), "view")
  
  # no selection puts everything into rowData
  se_olive <- as_sightse(olive, 
                         traveller = tourr::grand_tour(),
  )
  expect_s4_class(se_olive, "SightSE")
  expect_true(is(se_olive, "SingleCellExperiment"))
  expect_true(is(se_olive@traveller, "tour_path"))
  expect_equal(dim(assay(se_olive)), c(nrow(olive), 0))
  expect_identical(
    as.data.frame(rowData(se_olive)), olive 
  )
  
  # selecting throws an error if non-homegenous types
  expect_error(as_sightse(olive, 
                          traveller = tourr::grand_tour(),
                          1:ncol(olive)))
  
  # matrix constructor 
  sphere <- generate_sphere(1000, 10, 0, 1)
  se_sphere <- as_sightse(sphere,
                          traveller = tourr::grand_tour()
                          )
  expect_s4_class(se_sphere, "SightSE")
  expect_true(is(se_sphere, "SingleCellExperiment"))
  expect_true(is(se_sphere@traveller, "tour_path"))
  expect_identical(assay(se_sphere), sphere)
})
