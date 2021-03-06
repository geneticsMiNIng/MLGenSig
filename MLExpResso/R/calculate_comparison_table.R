#' @title Results from test for two datasets.
#'
#' @description Function \code{calculate_comparison_table} produces a dataset containing p-values and folds from tests evaluated on two datasets e.g methylation or expression. In addition, it produces an importance ranking column, which is the geometric mean of p-values from both tests and a column with a number of probes related to the gene.
#'
#'
#' @param data1 First dataset, suitable for \code{test1}.
#' @param data2 Second dataset, suitable for \code{test2}.
#' @param condition1 Condition for first dataset.
#' @param condition2 Condition for second dataset.
#' @param test1 Type of test for first dataset.
#' @param test2 Type of test for second dataset.
#' @param genom.data Data frame which contains information about CpG probes and corresponding genes, by default in our package we use \code{\link{illumina_humanmethylation_27_data}}.
#' @param genes.col Number of column in genom.data containing informations about genes (genes symbols).
#'
#' @return Data frame containing logatithm of fold and p-values from chosen tests.
#'
#' @seealso \code{\link{calculate_test}}
#'
#' @importFrom dplyr left_join
#'
#' @examples
#' \dontrun{
#' library(MLExpRessoData)
#' condition_exp <- ifelse(BRCA_exp$SUBTYPE == "LumA","LumA", "other")
#' condition_met <- ifelse(BRCA_met$SUBTYPE == "LumA","LumA", "other")
#'
#' BRCA_methylation_gen <- aggregate_probes(BRCA_met)
#'
#' data_met <- BRCA_methylation_gen
#' data_exp <- BRCA_exp
#' compare <- calculate_comparison_table(data_exp,data_met,cond_exp,cond_met, "nbinom2", "ttest")
#' }
#'
#' @export

calculate_comparison_table <- function(data1, data2, condition1, condition2, test1, test2, genom.data =  MLExpResso::illumina_humanmethylation_27_data, genes.col=11) {
  result1 <- calculate_test(data1, condition1, test1)
  result2 <- calculate_test(data2, condition2, test2)

  result1 <- result1[, c(1, 2, 3)]
  result2 <- result2[, c(1, 2, 3)]
  colnames(result1)[c(2, 3)] <- paste(test1, colnames(result1)[c(2, 3)], sep = ".")
  colnames(result2)[c(2, 3)] <- paste(test2, colnames(result2)[c(2, 3)], sep = ".")


  result <- merge(result1, result2, by = "id")
  result$geom.mean.rank <- sqrt(result[, 3] * result[, 5])

  # no_probes <- data.probes[, which(colnames(data.probes) %in% data.cols)]
  no_probes <- as.data.frame(table(genom.data[, genes.col]))
  colnames(no_probes)[1] <- "id"
  colnames(no_probes)[2] <- "no.probes"

  result <- left_join(result, no_probes, by = "id")
  result <- result[order(result$geom.mean.rank), ]


  return(result)
}
