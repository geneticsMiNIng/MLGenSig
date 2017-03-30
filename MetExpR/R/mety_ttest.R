#' @title mety_ttest: t test for metylation per each gene.
#'
#' @description Function \code{mety_ttest} computes statistics and p-values for metylation data mapped to genes.
#'
#' @param data data frame of zeros and ones. Rows coresponds to genes, columns to samples.
#' @param condition vector of levels coresponding to order of samples in data.
#'
#' @return A data frame with the following columns:
#'  \item{met.log.fold}{The log of the fold change.}
#'  \item{met.mean}{The base mean.}
#'  \item{met.t.stat}{The value of the t-test statistic.}
#'  \item{met.pval}{The p value for rejecting the null hypothesis.}
#'  \item{met.padj}{The adjusted p values}
#'  \item{met.B}{something...}
#'  \item{id}{he ID of the observable, taken from the row names of the counts slots.}
#'
#' @importFrom limma lmFit
#' @importFrom limma eBayes
#' @importFrom limma contrasts.fit
#' @importFrom limma topTable
#' @importFrom limma makeContrasts
#' @importFrom DESeq estimateDispersions
#' @importFrom DESeq estimateSizeFactors
#' @importFrom DESeq nbinomTest
#' @importFrom DESeq newCountDataSet
#'
#' @export

mety_ttest <-function(data, condition){
  design <- con_to_des(condition)
  fit <- lmFit(data, design)
  form <- paste0(colnames(design)[1],"-",colnames(design)[2])
  contMatrix <- makeContrasts(form, levels=design)
  fit2 <- contrasts.fit(fit, contMatrix)
  fit2 <- eBayes(fit2)
  DMPs <- topTable(fit2, number=Inf, coef=1)
  DMPs$id <- rownames(DMPs)
  colnames(DMPs) <- c("met.log.fold","met.mean","met.t.stat","met.pval","met.padj","met.B","id")
  return(DMPs)
}
