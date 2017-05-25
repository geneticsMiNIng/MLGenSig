#'@title chromosome_data
#'
#'@description Function \code{chromosome_data} produces a dataset for chosen chromosome from methylation data.
#'
#'@param data methylation dataset
#'@param number number of chromosome
#'
#'@return dataset containing CpG islands from all methylation data.
#'@importFrom dplyr filter
#'
#'@examples
#'chromosome_17 <- chromosome_data(BRCA_methylation_data_all, 17)
#'
#'@export

chromosome_data <- function(data, number){
  islands <- illumina_humanmethylation_27_data[which(illumina_humanmethylation_27_data$Chr==number),]
  data2 <- data[,which(colnames(data) %in% islands$Name)]
  cols <- colnames(data)
  cols <- as.data.frame(cols)
  cols$part <- substr(cols$cols,1,2)
  cols$logical <- (cols$part!="cg")
  new <- filter(cols,logical==TRUE)
  names <-new$cols
  names<- droplevels(names)
  cols <- data[,names]
  data2 <- cbind(cols,data2)
  return(data2)
}
