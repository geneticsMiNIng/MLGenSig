#' @title test_edger
#'
#' @description Function \code{test_edger} computes the statistics for quasi-likelihood F-tests or likelihood ratio tests .
#'
#' @param data data frame containing the raw counts of sequencing reads. Columns corresponds to genes, rows to samples.
#' @param condition vector of levels coresponding to order of samples in data.
#' @param type type of test "lrt" for likelihood ratio tests, "qlf" for quasi-likelihood F-tests.
#'
#' @return A data frame with the following columns:
#'  \item{id}{The ID of the observable, taken from the row names of the counts slots.}
#'  \item{log2.fold}{The log2 of the fold change.}
#'  \item{pval}{The p-values for rejecting the null hypothesis about the means equality.}

#'@importFrom edgeR DGEList
#'@importFrom edgeR calcNormFactors
#'@importFrom edgeR estimateDisp
#'@importFrom edgeR glmFit
#'@importFrom edgeR glmLRT
#'@importFrom edgeR glmQLFit
#'@importFrom edgeR glmQLFTest


test_edger <- function(data, condition, type="lrt", ...){
  y <- DGEList(counts=data,group=condition)
  y <- calcNormFactors(y)
  design <- model.matrix(~condition)
  y <- estimateDisp(y,design)
  if(type=="lrt"){
    fit <- glmFit(y,design, ...)
    lrt <- glmLRT(fit,coef=2)
    result <- lrt@.Data[[14]]
  }
  if(type=="qlf"){
    fit <- glmQLFit(y,design, ...)
    qlf <- glmQLFTest(fit,coef=2)
    result <- qlf@.Data[[17]]
  }
  result$id <- rownames(result)
  colnames(result)[1] <- "log2.fold"
  colnames(result)[4] <- "pval"
  result <- result[,c(5,1,4)]
  rownames(result) <- 1:nrow(result)

  return(result)

}
