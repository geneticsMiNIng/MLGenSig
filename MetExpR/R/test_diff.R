#' @title test_diff
#'
#' @description Function \code{test_diff} computes statistics and p-values for choosen test.
#'
#' @param data data frame of ... Columns corresponds to genes, rows to samples.
#' @param condition vector of levels coresponding to order of samples in data.
#' @param test values: ttest, nbinom.
#'
#' @return A data frame with the following columns:
#'  \item{id}{The ID of the observable, taken from the row names of the counts slots}
#'  \item{mean}{The base mean}
#'  \item{log.fold}{The log2 of the fold change}
#'  \item{pval}{The p values for rejecting the null hypothesis such that means in both groups are equal}
#'  \item{padj}{The adjusted p values}
#'
#' @export

test_diff <- function(data, condition, test="ttest"){
  if(test=="ttest"){
     res <- mety_ttest(data, condition)
     res <- res[,c(7,2,1,4,5)]
  }
  if(test=="nbinom"){ 
    res <- expr_nbinom(data, condition)
    res <- res[,c(1,2,6,7,8)]
  }
  return(res)
}
