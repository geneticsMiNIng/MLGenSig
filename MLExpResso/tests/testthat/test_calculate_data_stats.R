library(MLExpRessodata)
condition_m <- ifelse(BRCA_methylation_chr17=="LumA", "LumA", "other")

test_that("Input",{
  expect_error(calculate_data_stats())
  expect_error(calculate_data_stats(condition_m))
  
})

test_that("Output",{
  expect_is(calculate_data_stats(BRCA_methylation_chr17, condition_m), "data.frame")
  expect_equal(nrow(calculate_data_stats(BRCA_methylation_chr17, condition_m)),1)
  expect_equal(ncol(calculate_data_stats(BRCA_methylation_chr17, condition_m)),4)
})