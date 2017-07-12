#'@title Basic statistics for gene.
#'
#'@description Function \code{gene_stats} generate a data frame containing basic statistics for chosen gene.
#'
#'@param data data frame containing genes expression or methylation mapped to gene.
#'@param condition vector of levels of groups coresponding to order of samples in data.
#'@param gene symbol of chosen gene.
#'@param round num
#'
#'@return data frame with following columns:
#'  \item{min}{minimal value for chosen gene in group}
#'  \item{1st Q}{first quartile}
#'  \item{med}{median of gene values}
#'  \item{mean}{mean for gene values}
#'  \item{3rd Q}{third quartile}
#'  \item{max}{maximal value for chosen gene in group}
#'

gene_stats <-function(data, condition, gene, round=2){
  data_n <- cbind(data,condition)
  result <- tapply(data_n[[gene]],data_n$condition, summary)
  result_df <- data.frame()
  for(i in names(result)){
    result_df <- rbind(result_df, result[[i]])
  }
  colnames(result_df) <- c( "min", "1st Q", "med", "mean", "3rd Q",  "max")
  result_df$count <- table(condition)
  rownames(result_df) <- names(result)
  result_df <- round(result_df, round)
  return(t(result_df))
}
