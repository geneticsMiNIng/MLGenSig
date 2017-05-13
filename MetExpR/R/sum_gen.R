#'@title boxplot_gene_expr
#'
#'@description Function \code{boxplot_gene_expr} generate a boxplot of expression for choosen gene.
#'
#'@param data data frame containing genes expression.
#'@param condition name of gene which expression we want to visualise.
#'@param gen s
#'
#'@return boxplot of expression.
#'
#'@seealso genereg_vs_met

#'
#'@export

sum_gen <-function(data, condition, gen){
  data_n <- cbind(data,condition)
  result <- tapply(data_n[[gen]],data_n$condition, summary)
  result_df <- data.frame()
  for(i in names(result)){
    result_df <- rbind(result_df, result[[i]])
  }
  colnames(result_df) <- c( "min", "1st Q", "med", "mean", "3rd Q",  "max")
  result_df$count <- table(condition)
  rownames(result_df) <- names(result)
  result_df <- round(result_df, 1)
  return(t(result_df))
}
