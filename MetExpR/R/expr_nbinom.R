#' @title expr_nbinom:  for gene expression
#'
#' @description Function \code{expr_nbinom} computes statistics and p-values for negative binomial Test
#'
#' @param data data frame of zeros and ones. Rows coresponds to gene, columns to samples.
#' @param condition vector of levels coresponding to order of samples in data.
#'
#' @return A data frame with the following columns:
#'
#' @export

expr_nbinom <- function(data, condition){
  levels <- unique(condition)
  cds = newCountDataSet( data, condition )
  #normalisation
  cds = estimateSizeFactors( cds )
  #variance
  cds = estimateDispersions( cds )
  #negative binomial test
  res = nbinomTest( cds, levels[1], levels[2])
  return(res)
}
