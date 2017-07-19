#'@title Results from test for two datasets.
#'
#'@description Function \code{calculate_comparison_table} produces a dataset containing p-values and folds from tests evaluaetes on two datasets e.g methylation or expression.
#'
#'
#'@param data1 first dataset, suitable for \code{test1}
#'@param data2 second dataset, suitable for \code{test2}
#'@param cond1 condition for first dataset
#'@param cond2 condition for second dataset
#'@param test1 type of test for first dataset
#'@param test2 type of test for second dataset
#'@param data.probes data frame which contains information about CpG probes and corresponding genes, by default in our package we use `illumina human methylation` 
#'@param aggregate.col number of column in data.probes containing informations about genes (genes symbols)
#'
#'@return data frame containing logatithm of fold and p-values from chosen tests
#'
#'@seealso \code{\link{calculate_test}}
#'
#'@importFrom dplyr left_join
#'
#'@examples
#'\dontrun{
#'library(MLExpRessodata)
#'condition_exp <- ifelse(BRCA_mRNAseq_chr17$SUBTYPE=="LumA","LumA", "other")
#'condition_met <- ifelse(BRCA_methylation_chr17$SUBTYPE=="LumA","LumA", "other")
#'
#'BRCA_methylation_gen <- aggregate_probes(BRCA_methylation_chr17)
#'
#'compare <- calculate_comparison_table(BRCA_mRNAseq_chr17, BRCA_methylation_gen_17, condition_exp, condition_met, test1="nbinom2", test2="ttest")
#'}
#'
#'@export

calculate_comparison_table <- function(data1, data2, cond1, cond2, test1, test2, data.probes = illumina_humanmethylation_27_data, aggregate.col=11){
result1 <- calculate_test(data1, cond1,test1)
result2 <- calculate_test(data2, cond2,test2)

result1 <- result1[,c(1,3,4)]
result2 <- result2[,c(1,3,4)]
colnames(result1)[c(2,3)] <- paste(test1,colnames(result1)[c(2,3)], sep=".")
colnames(result2)[c(2,3)] <- paste(test2,colnames(result2)[c(2,3)], sep=".")


result <- merge(result1, result2, by="id")
result$geom.mean.rank <- sqrt(result[,3]*result[,5])

#no_probes <- data.probes[, which(colnames(data.probes) %in% data.cols)]
no_probes <- as.data.frame(table(data.probes[,aggregate.col]))
colnames(no_probes)[1] <- "id"
colnames(no_probes)[2] <- "No.probes"

result <- left_join(result, no_probes, by="id")
result <- result[order(result$geom.mean.rank),]


return(result)
}
