#'Function creates a full set for metylation and expression which contains values from functions `mety_ttest` i `expr_nbinom` 
#'
#'@param dt1 data.frame, a result of `expr_nbinom` function  
#'@param dt2 data.frame, a result of `mety_ttest` function
#'
#'@return data.frame which contains all statistics from both functions
#'
#'@export

full_data <- function(dt1,dt2){
  test_expr <- dt1[which(dt1$id %in% rownames(dt2)) ,]
  test_metyl <- dt2[which( rownames(dt2) %in% dt1$id) ,]
  
  data <- merge(test_expr,test_metyl,by="id")
  return(data)
  
}
