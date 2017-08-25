#' @title Statistical computations for methylation and expression data.
#'
#' @description Function \code{calculate_test} computes log folds and p-values for choosen test for data from methylation or expression.
#' The function uses: t-test, negative binomial test, likelihood-ratio test(LRT), quasi-likelihood F-test(QLF).
#' By default function calls the \code{t-test}.
#'
#' @param data Object of the class appropriate for the given test. More in \code{details} section.
#' @param condition factor of levels coresponding to order of samples in data.
#' @param test variable defining test. Values: ttest, nbinom, nbinom2, lrt, qlf, methyanalysis. More in \code{details} section.
#' @param ... other parameters e.g. `adjust.method` for argument `ttest`
#'
#' @return A data frame with the following columns:
#'  \item{id}{The id of the observable, taken from the row names of the counts slots.}
#'  \item{log2.fold}{The log2 of the fold change.}
#'  \item{pval}{The p-values for rejecting the null hypothesis about the means equality.}
#'  \item{mean}{Column correspond to means for each gene defined by condition and mean for all probes.}
#'
#' @details Each test may require different data. In this section we will describe details for each availible test:
#' \describe{
#'   \item{ttest}{
#'   Student's t-test \cr
#'   Test for expression and methylation.\cr
#'   This test is based on function \code{\link[limma]{lmFit}} from \code{limma} package. \cr
#'   For this test you should aggregate CpG probes to genes using function: \code{\link{aggregate_probes}}.
#'   Data for this test should have following structure: columns correspond to genes, rows to samples.
#'   }
#'   \item{nbinom}{
#'   Negative binomial test\cr
#'   Test for expression.\cr
#'   This test is based on function \code{\link[DESeq]{nbinomTest}} from \code{DESeq} package. \cr
#'   Data for this test should have following structure: columns correspond to genes, rows to samples. \cr
#'   Calculations may take some time. It is suggested to use \code{nbinom2} parameter.
#'   }
#'   \item{nbinom2}{
#'   Negative binomial test\cr
#'   Test for expression.\cr
#'   This test is based on function \code{\link[DESeq2]{DESeq}} from \code{DESeq2} package. \cr
#'   Data for this test should have following structure: columns correspond to genes, rows to samples.
#'   }
#'   \item{lrt}{
#'   Likelihood-ratio test (LRT)\cr
#'   Test for expression.\cr
#'   This test is based on function \code{\link[edgeR]{glmFit}} and \code{\link[edgeR]{glmLRT}} from \code{edgeR} package. \cr
#'   Data for this test should have following structure: columns correspond to genes, rows to samples.
#'   }
#'   \item{qlf}{
#'   Quasi-likelihood F-test (QLF)\cr
#'   Test for expression.\cr
#'   This test is based on functions \code{\link[edgeR]{glmQLFit}} and \code{\link[edgeR]{glmQLFTest}} from \code{edgeR} package. \cr
#'   Data for this test should have following structure: columns correspond to genes, rows to samples.
#'   }
#'   \item{methyanalysis}{
#'   Slide window smoothing\cr
#'   Test for methylation.\cr
#'   This test is based on function \code{\link[methyAnalysis]{detectDMR.slideWin}} from \code{methyAnalysis} package.
#'   It requires a special class of argument data - \code{MethyGenoSet}.
#'   }
#' }
#'
#' @seealso \code{\link{calculate_comparison_table}, \link{aggregate_probes}}
#'
#'
#' @examples
#' \dontrun{
#'
#' library(MLExpRessoData)
#' BRCA_methylation_gene <- aggregate_probes(BRCA_methylation_all)
#'
#' condition_m <- ifelse(BRCA_methylation_all$SUBTYPE == "LumA", "LumA", "other")
#' test_methylation <- calculate_test(BRCA_methylation_gene, condition_m, "ttest")
#'
#' }
#'
#' @export

calculate_test <- function(data, condition, test="ttest", ...) {
  condition <- make.names(condition)
  if (test == "ttest") {
    res <- test_tstudent(data, condition, ...)
    res <- res[, c(1, 3, 4)]
  }
  if (test == "nbinom") {
    res <- test_nbinom(data, condition, ...)
    res <- res[, c(1, 3, 4)]
  }
  if (test == "nbinom2") {
    res <- test_nbinom2(data, condition, ...)
    res <- res[, c(7, 2, 5)]
  }
  if (test == "lrt" || test == "qlf") {
    res <- test_edger(t(data), condition, type = test, ...)
  }
  if (test == "methyanalysis") {
    res <- test_methyanalysis(data, condition, ...)
    res <- res[, c(1, 4, 5)]
  }
  colnames(res) <- c("id", "log2.fold", "pval")

  means <- calculate_condition_means(data, condition)
  res <- merge(res, means, by = "id")
  res <- res[order(res$pval), ]
  rownames(res) <- NULL
  return(res)
}
