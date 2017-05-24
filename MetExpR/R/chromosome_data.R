#'@title chromosome_data
#'
#'@description Function \code{chromosome_data} produces a dataset for chosen chromosome from methylation data.
#'
#'@param data methylation dataset
#'@param number number of chromosome
#'
#'@return dataset containing CpG islands from all methylation data.
#'@importFrom dplyr filter
#'@export

chromosome_data <- function(data, number){
  islands <- illumina_humanmethylation_27_data[which(illumina_humanmethylation_27_data$Chr==number),]
  data2 <- data[,which(colnames(data) %in% islands$Name)]
  xx <- colnames(data)
  xx <- as.data.frame(xx)
  xx$skr <- substr(xx$xx,1,2)
  xx$log <- (xx$skr!="cg")
  bbb <- filter(xx,log==TRUE)
  names <-bbb$xx
  names<- droplevels(names)
  cols <- data[,names]
  data2 <- cbind(cols,data2)
  return(data2)
}
