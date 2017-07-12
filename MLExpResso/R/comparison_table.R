#'@title Results from test for two datasets.
#'
#'@description Function \code{compare_table()} produces a dataset containing p-values and folds from tests evaluaetes on two datasets e.g methylation or expression.
#' 
#'
#'@param data1 first dataset, suitable for \code{test1}
#'@param data2 second dataset, suitable for \code{test2}
#'@param cond1 condition for first dataset
#'@param cond2 condition for second dataset
#'@param test1 type of test for first dataset
#'@param test2 type of test for second dataset
#'
#'@return data.frame containing logatithm of fold and p-values from chosen tests
#'
#'@export

comparison_table <- function(data1, data2, cond1, cond2, test1, test2){
result1 <- test_diff(data1, cond1,test1)
result2 <- test_diff(data2, cond2,test2)
result1 <- result1[,c(1,3,4)]
result2 <- result2[,c(1,3,4)]
colnames(result1)[c(2,3)] <- paste(test1,colnames(result1)[c(2,3)], sep=".")  
colnames(result2)[c(2,3)] <- paste(test2,colnames(result2)[c(2,3)], sep=".")  


result <- merge(result1, result2, by="id")

return(result)
}