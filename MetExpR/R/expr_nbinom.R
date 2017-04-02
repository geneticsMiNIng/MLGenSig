#' @title expr_nbinom:  for gene expression
#'
#' @description Function \code{expr_nbinom} computes statistics and p-values for negative binomial Test
#'
#' @param data data frame containing counts of ... Columns corresponds to genes, rows to samples.
#' @param condition vector of levels coresponding to order of samples in data.
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
#' @importFrom DESeq estimateDispersions
#' @importFrom DESeq estimateSizeFactors
#' @importFrom DESeq nbinomTest
#' @importFrom DESeq newCountDataSet
#'
#' @export

expr_nbinom <- function(data, condition){
  data<-t(data)
  levels <- unique(condition)
  cds <- newCountDataSet( data, condition )
  #normalisation
  cds <- estimateSizeFactors( cds )
  #variance
  cds <- estimateDispersions( cds )
  #negative binomial test
  res <- nbinomTest( cds, levels[1], levels[2])
  colnames(res) <- c("id","exp.mean","exp.mean.gr1","exp.mean.g2","exp.fold","exp.log.fold","exp.pval","exp.padj")
  return(res)
}
