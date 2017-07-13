#' @title Statistical computations for methylation or expression data.
#'
#' @description Function \code{calculate_test} computes log folds and p-values for choosen test.
#' The function uses: t-test, negative binomial test, likelihood-ratio test(LRT), quasi-likelihood F-test(QLF).
#' By default function calls the \code{ttest}.
#'
#' @param data Object of the class appropriate for the given test. More in \code{details} section.
#' @param condition factor of levels coresponding to order of samples in data.
#' @param test variable defining test. Values: ttest, nbinom, nbinom2, lrt, qlf. More in \code{details} section.
#'
#' @return A data frame with the following columns:
#'  \item{id}{The id of the observable, taken from the row names of the counts slots.}
#'  \item{mean}{The base mean.}
#'  \item{log.fold}{The log2 of the fold change.}
#'  \item{pval}{The p-values for rejecting the null hypothesis about the means equality.}
#'  \item{padj}{The adjusted p-values.}
#'
#'@details Each test may require different data. In this section we will describe details for each availible test:
#' \describe{
#'   \item{ttest}{
#'   Student's t-test
#'   \code{\link[limma]{lmFit}}
#'   }
#'   \item{nbinom}{
#'    negative binomial test
#'   Based on function \code{\link[DESeq]{nbinomTest} from DESeq package.}
#'  Calculations may take some time. It is suggested to use \code{nbinom2} parameter.
#'   }
#'   \item{nbinom2}{
#'    negative binomial test
#'   Based on \code{\link[DESeq2]{DESeq} from DESeq2 package.}
#'   }
#'   \item{lrt}{
#'   likelihood-ratio test(LRT)
#'   based on function \code{\link[edgeR]{glmLRT}}
#'   }
#'   \item{qlf}{
#'   quasi-likelihood F-test(QLF)
#'   based on functions \code{\link[edgeR]{glmQLFit}} and \code{\link[edgeR]{glmQLFTest}}
#'   }
#' }
#'more about data classes, references to Bioconductor packages
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
