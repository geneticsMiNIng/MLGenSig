#' @title Negative binomial test for gene expression
#'
#' @description Function \code{test_nbinom} computes statistics and p-values for negative binomial test.
#'
#' @param data data frame containing the raw counts of sequencing reads. Columns corresponds to genes, rows to samples.
#' @param condition vector of levels coresponding to order of samples in data.
#'
#' @return A data frame with the following columns:
#'  \item{id}{The ID of the observable, taken from the row names of the counts slots.}
#'  \item{exp.mean}{The base mean.}
#'  \item{exp.log.fold}{The log2 of the fold change.}
#'  \item{exp.pval}{The p-values for rejecting the null hypothesis about the means equality.}
#'  \item{exp.padj}{The adjusted p-values (adjusted with 'p.adjust( pval, method="BH")')}
#'  \item{exp.mean.gr1}{The base mean for the counts for the first condition.}
#'  \item{exp.mean.gr2}{The base mean for the counts for the second condition.}
#'  \item{exp.fold}{The ratio mean.gr1/mean.gr2.}
#'   
#' @importFrom DESeq estimateDispersions
#' @importFrom DESeq estimateSizeFactors
#' @importFrom DESeq nbinomTest
#' @importFrom DESeq newCountDataSet
#'
#'@seealso mety_t


test_nbinom <- function(data, condition){
  data<-t(data)
  levels <- unique(condition)
  cds <- newCountDataSet( data, condition )
  #normalisation
  cds <- estimateSizeFactors( cds )
  #variance
  cds <- estimateDispersions( cds )
  #negative binomial test
  res <- nbinomTest( cds, levels[1], levels[2])
  colnames(res) <- c("id","mean","mean.gr1","mean.gr2","fold","log.fold","pval","padj")
  res <- res[,c(1,2,6,7,8,3,4,5)]
  return(res)
}
