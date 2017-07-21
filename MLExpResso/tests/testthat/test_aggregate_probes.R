library(MLExpRessodata)

test_that("Input",{
  expect_error(aggregate_probes())
  expect_error(aggregate_probes(keep="SUBTYPE"))
})

test_that("Output",{
  expect_is(aggregate_probes(BRCA_methylation_chr17, keep="SUBTYPE"), "data.frame")
})