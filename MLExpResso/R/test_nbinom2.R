#' @title expr_nbinom2: negative binomial test for gene expression
#'
#' @description Function \code{test_nbinom} computes statistics and p-values for negative binomial test.
#'
#' @param data data frame containing the raw counts of sequencing reads. Columns corresponds to genes, rows to samples.
#' @param condition vector of levels coresponding to order of samples in data.
#' @param ... some additional parameters
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
#' @importFrom DESeq2 DESeqDataSetFromMatrix
#' @importFrom DESeq2 DESeq
#' @importFrom DESeq2 results
#'
#'@seealso mety_t
#'


test_nbinom2 <- function(data, condition, ...){
  data <- t(data)
  condition <- as.data.frame(condition)
  rownames(condition) <- colnames(data)

  datamx <- DESeqDataSetFromMatrix(countData = data,
                                   colData = condition,
                                   design = ~ condition)
  dds <- DESeq(datamx, ...)
  res <- results(dds)
  res2 <- as.data.frame(res@listData)
  rownames(res2) <- res@rownames
  res2$id <- rownames(res2)
  return(res2)
}
