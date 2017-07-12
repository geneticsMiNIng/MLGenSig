#' @title test_diff: statistical computations for data from methylation or expression.
#'

#' @description Function \code{calculate_test} computes statistics and p-values for choosen test. The function can use two test: t-test for methylation data and negative binomial test for data from gene expression. By default function \code{test_diff} calls the \code{mety_ttest} for methylation.
#'
#' @param data data frame containing values of methylation or the raw counts of sequencing reads. Columns corresponds to genes, rows to samples.
#' @param condition vector of levels coresponding to order of samples in data.
#' @param test variable defining test. Values: ttest, nbinom.
#'
#' @return A data frame with the following columns:
#'  \item{id}{The ID of the observable, taken from the row names of the counts slots.}
#'  \item{mean}{The base mean.}
#'  \item{log.fold}{The log2 of the fold change.}
#'  \item{pval}{The p-values for rejecting the null hypothesis about the means equality.}
#'  \item{padj}{The adjusted p-values.}
#'
#' @export

calculate_test <- function(data, condition, test="ttest"){
  if(test=="ttest"){
     res <- test_tstudent(data, condition)

     res <- res[,c(1,2,3,4,5)]
     colnames(res) <- c("id","mean","log2.fold","pval","padj")
 }
  if(test=="nbinom"){
    res <- test_nbinom(data, condition)
    res <- res[,c(1,2,3,4,5)]
    colnames(res) <- c("id","mean","log2.fold","pval","padj")
  }
  if(test=="nbinom2"){
    res <- test_nbinom2(data, condition)
    res <- res[,c(7,1,2,5,6)]
    colnames(res) <- c("id","mean","log2.fold","pval","padj")

  }

  res <- res[,c(1,2,3,4,5)]
  colnames(res) <- c("id","mean","log2.fold","pval","padj")
  if(test=="lrt"){
    res<- test_edger(data, condition, type="lrt")
  }
  if(test=="qlf"){
    res <- test_edger(data, condition, type="qlf")
  }

  return(res)
}
