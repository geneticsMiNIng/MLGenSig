#' @title t test for methylation per each gene
#'
#' @description Function \code{test_tstudent} computes statistics and p-values for methylation data mapped to genes.
#'
#' @param data data frame containing values of methylation: columns coresponds to genes, rows to samples.
#' @param condition vector of levels coresponding to order of samples in data.
#' @param ... some additional parameters
#'
#' @return A data frame with the following columns:
#'  \item{id}{The ID of the observable, taken from the row names of the counts slots.}
#'  \item{mean}{The base mean.}
#'  \item{log.fold}{The log of the fold change.}
#'  \item{pval}{The p-values for rejecting the null hypothesis about the means equality.}
#'  \item{padj}{The adjusted p-values.}
#'  \item{t.stat}{The value of the t-test statistic.}
#'  \item{B}{The log-odds that that gene is differentially expressed.}
#'
#' @importFrom limma lmFit
#' @importFrom limma eBayes
#' @importFrom limma contrasts.fit
#' @importFrom limma topTable
#' @importFrom limma makeContrasts
#'
#' @seealso expr_nbinom
#' @keywords internal

test_tstudent <- function(data, condition, ...) {
  data <- t(data)
  design <- con_to_des(condition = condition)
  fit <- lmFit(data, design)
  forms <- paste0(colnames(design)[1], "-", colnames(design)[2])
  contMatrix <- makeContrasts(contrasts = forms, levels = design)
  fit2 <- contrasts.fit(fit, contMatrix)
  fit2 <- eBayes(fit2)
  DMPs <- topTable(fit2, number = Inf, coef = 1, ...)
  DMPs$id <- rownames(DMPs)
  colnames(DMPs) <- c("log.fold", "mean", "t.stat", "pval", "padj", "B", "id")
  DMPs <- DMPs[, c(7, 2, 1, 4, 5, 3, 6)]
  return(DMPs)
}
