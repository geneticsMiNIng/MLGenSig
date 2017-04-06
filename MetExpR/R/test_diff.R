#' @title test_diff: statistical computations for data from methylation or expression
#'
#' @description Function \code{test_diff} computes statistics and p-values for choosen test. The function can use two test: t-test for methylation data and negative binomial test for data from gene expression. By default function \code{test_diff} calls the \code{mety_ttest} for methylation. 
#'
#' @param data data frame containing values of methylation or the raw counts of sequencing reads. Columns corresponds to genes, rows to samples.
#' @param condition vector of levels coresponding to order of samples in data.
#' @param test values: ttest, nbinom.
#'
#' @return A data frame with the following columns:
#'  \item{id}{The ID of the observable, taken from the row names of the counts slots.}
#'  \item{mean}{The base mean.}
#'  \item{log.fold}{The log2 of the fold change.}
#'  \item{pval}{The p-values for rejecting the null hypothesis about the means equality.}
#'  \item{padj}{The adjusted p-values.}
#'
#' @seealso expr_binom, mety_ttest
#' @export

test_diff <- function(data, condition, test="ttest"){
  if(test=="ttest"){
     res <- mety_ttest(data, condition)
     res <- res[,c(1,2,3,4,5)]
     colnames(res) <- c("id","mean","log.fold","pval","padj")
  }
  if(test=="nbinom"){ 
    res <- expr_nbinom(data, condition)
    res <- res[,c(1,2,6,7,8)]
    colnames(res) <- c("id","mean","log.fold","pval","padj")
  }
  
}
