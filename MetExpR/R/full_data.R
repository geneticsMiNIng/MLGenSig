#'Function creates a full set for metylation and expression which contains values from functions `mety_ttest` i `expr_nbinom` 
#'
#'@param dt_expr data.frame, a result of `expr_nbinom` function  
#'@param dt_met data.frame, a result of `mety_ttest` function
#'
#' @return A data frame with the following columns from both functions:
#'  \item{id}{The ID of the observable, taken from the row names of the counts slots.}
#'  \item{mean}{The base mean.}
#'  \item{log.fold}{The log of the fold change.}
#'  \item{pval}{The p-values for rejecting the null hypothesis about the means equality.}
#'  \item{padj}{The adjusted p-values.}
#'
#'@export

full_data <- function(dt_expr,dt_met){
  test_expr <- dt_expr[which(dt_expr$id %in% rownames(dt_met)) ,]
  test_metyl <- dt_met[which( rownames(dt_met) %in% dt_expr$id) ,]
  
  test_expr <- test_expr[,which(colnames(test_expr) %in% c("id","mean","log.fold","pval","padj"))]
  test_metyl <- test_metyl[,which(colnames(test_metyl) %in% c("id","mean","log.fold","pval","padj"))]
  
  data <- merge(test_expr,test_metyl,by="id")
  
  colnames(data) <- c("id","exp.mean","exp.log.fold","exp.pval","exp.padj","met.mean","met.log.fold","met.pval","met.padj")
  
  return(data)
  
}
