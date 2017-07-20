#library(methyAnalysis)

#data(exampleMethyGenoSet)
#exampleMethyGenoSet <- exampleMethyGenoSet
#qsampleType <- colData(exampleMethyGenoSet)$SampleType

#test_that("Input",{
#  expext_error(test_methyanalysis())
#  expect_error(test_methyanalysis(exampleMethyGenoSet))
#  expect_error(test_methyanalysis(sampleType))
#})
  
#test_that("Output",{
#  expect_is(test_methyanalysis(exampleMethyGenoSet,sampleType), "data.frame")
#  expect_equal(nrow(test_methyanalysis(exampleMethyGenoSet,sampleType)),45)
#  expect_equal(ncol(test_methyanalysis(exampleMethyGenoSet,sampleType)),6)
#  
#})