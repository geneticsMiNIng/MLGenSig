#' @title mety_ttest: t test for metylation per each gene.
#'
#' @description Function \code{mety_ttest} computes statistics and p-values for metylation data mapped to genes.
#'
#' @param data data frame of zeros and ones. Rows coresponds to genes, columns to samples.
#' @param condition vector of levels coresponding to order of samples in data.
#'
#' @return A data frame with the following columns:
#'  \item{met.log.fold}{a}
#'  \item{met.mean}{a}
#'  \item{met.t.stat}{a}
#'  \item{met.pval}{a}
#'  \item{met.padj}{a}
#'  \item{met.B}{a}
#'  \item{id}{a}
#'
#' @export

mety_ttest <-function(data, condition){
  design <- con_to_des(condition)
  fit <- lmFit(data, design)
  form <- paste0(colnames(design)[1],"-",colnames(design)[2])
  contMatrix <- makeContrasts(form, levels=design)
  fit2 <- contrasts.fit(fit, contMatrix)
  fit2 <- eBayes(fit2)
  DMPs <- topTable(fit2, num=Inf, coef=1)
  DMPs$id <- rownames(DMPs)
  colnames(DMPs) <- c("met.log.fold","met.mean","met.t.stat","met.pval","met.padj","met.B","id")
  return(DMPs)
}
