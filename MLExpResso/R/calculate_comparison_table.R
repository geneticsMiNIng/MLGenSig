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
#'
#'@return data frame containing logatithm of fold and p-values from chosen tests
#'
#'@examples
#'\dontrun{
#'compare <- calculate_comparison_table(BRCA_mRNAseq_chr17, BRCA_methylation_gen_17, condition_exp, condition_met, test1="nbinom2", test2="ttest")
#'}
#'
#'@export

calculate_comparison_table <- function(data1, data2, cond1, cond2, test1, test2){
result1 <- calculate_test(data1, cond1,test1)
result2 <- calculate_test(data2, cond2,test2)

result1 <- result1[,c(1,3,4)]
result2 <- result2[,c(1,3,4)]
colnames(result1)[c(2,3)] <- paste(test1,colnames(result1)[c(2,3)], sep=".")  
colnames(result2)[c(2,3)] <- paste(test2,colnames(result2)[c(2,3)], sep=".")  


result <- merge(result1, result2, by="id")
result$genes_rank <- sqrt(result[,3]*result[,5])

return(result)
}