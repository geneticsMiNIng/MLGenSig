library(MLExpRessodata)

data_e <- BRCA_mRNAseq_chr17[1:5,c(2:5)]
cond <- c("A","B","A","B","B")

test_that("Input",{
  expext_error(test_nbinom())
})

