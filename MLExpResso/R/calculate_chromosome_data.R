#'@title chromosome_data
#'
#'@description Function \code{calculate_chromosome_data} produces a dataset for chosen chromosome from methylation data.
#'
#'@param data methylation dataset
#'@param number number of chromosome
#'
#'@return dataset containing CpG islands from all methylation data.
#'@importFrom dplyr filter
#'
#'@examples
#'\dontrun{ 
#'library(MLExpRessoData)
#'chr_17 <- calculate_chromosome_data(BRCA_methylation_all, 17)
#'}
#'

calculate_chromosome_data <- function(data, number){
  illumina_humanmethylation_27_data <- MLExpResso::illumina_humanmethylation_27_data
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
  cols <- as.data.frame(cols)
  if(dim(cols)[2]==1){
    colnames(cols)[1] <- "SUBTYPE"
  }
  if(dim(cols)[2]==2){
    colnames(cols)<- c("id","survival_status")
  }
  data2 <- cbind(cols,data2)

  return(data2)
}
