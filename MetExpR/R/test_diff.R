#' @title test_diff
#'
#' @description Function \code{tet_diff} computes statistics and p-values for choosen test.
#'
#' @param data data frame of ... Columns corresponds to genes, rows to samples.
#' @param condition vector of levels coresponding to order of samples in data.
#' @param test values: ttest, nbinom.
#'
#' @return A data frame with the following columns:
#'  \item{id}{The ID of the observable, taken from the row names of the counts slots.}
#'  \item{exp.mean}{The base mean.}
#'  \item{exp.mean.gr1}{The base mean for the counts for the first condition.}
#'  \item{exp.mean.g2}{The base mean for the second condition.}
#'  \item{exp.fold}{The ratio mean1/mean2.}
#'  \item{exp.log.fold}{The log2 of the fold change.}
#'  \item{exp.pval}{The p value for rejecting the null hypothesis 'mean1==mean2'}
#'  \item{exp.padj}{The adjusted p values (adjusted with 'p.adjust( pval, method="BH")')}
#'
#' @export

test_diff <- function(data, condition, test="ttest"){
  if(test=="ttest") res = mety_ttest(data, condition)
  if(test=="nbinom") res = expr_nbinom(data, condition)
  return(res)
}
