#' @title t test for methylation per each gene from methyAnalysis package
#'
#' @description Function \code{test_methyanalysis} computes statistics and p-values for methylation data.
#'
#' @param data object of class MehtyGenoSet, consisting methylation for each CpG island.
#' @param condition vector of levels coresponding to order of samples in data.
#'
#' @return A data frame with the following columns:
#'  \item{id}{The ID of the observable, taken from the row names of the counts slots.}
#'  \item{name}{The CpG probe name.}
#'  \item{mean}{The base mean.}
#'  \item{log2.fold}{The 2-log of the fold change.}
#'  \item{pval}{The p-values for rejecting the null hypothesis about the means equality.}
#'  \item{padj}{The adjusted p-values.}
#'
#' @importFrom dplyr left_join
#' @importFrom methyAnalysis detectDMR.slideWin
#'
#'@seealso mety_ttest
#'


test_methyanalysis <- function(data, condition){

  result <- detectDMR.slideWin(data, sampleType=condition)
  result <- result@elementMetadata@listData
  result2 <- data.frame(result$PROBEID, result$difference, result$p.value, result$p.adjust, result$mean_Type1)
  colnames(result2)[1] <- "Name"
  genes <- illumina_humanmethylation_27_data[,c(1,11)]
  genes2 <- genes[result2$Name,]

  result2$Name <- as.character(result2$Name)
  genes2$Symbol <- as.character(genes2$Symbol)

  result3 <- left_join(result2, genes2, by="Name")
  result3 <- na.omit(result3)
  result4 <- result3 %>% group_by(Symbol)%>%summarize(pval = min(result.p.value))%>%
    inner_join(result3, by='Symbol')

  result5 <- result4[which(result4$pval==result4$result.p.value),]

  colnames(result5)[4] <- "diff"
  colnames(result5)[6] <- "p.adj"
  colnames(result5)[7] <- "mean"
  result5$log2.fold <- log2(abs(result5$diff/result5$mean))
  result5 <- result5[,-5]
  colnames(result5)[1] <- "id"
  colnames(result5)[3] <- "name"
  result5 <- result5[,c(1,3,6,7,2,5)]
  return(result5)

}
