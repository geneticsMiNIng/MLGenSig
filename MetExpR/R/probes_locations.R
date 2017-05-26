#'@title Locations of CpG probes
#'
#'@description dfd
#'
#'@param data d
#'@param gene d
#'
#'@return data frame
#'@importFrom reshape2 melt
#'@importFrom dplyr %>%


probes_locations <- function(data, gene){
  genom <- illumina_humanmethylation_27_data[,c(1,4,11)]
  HG18_coord <- genom[,c(1:2)]
  genom_data <- genom[genom$Symbol==gene,]
  genom_data_2<- data[,which(colnames(data)%in% genom_data$Name)]
  genom_data_2 <- cbind(genom_data_2,condition)
  melted_val <- genom_data_2 %>% melt(id.vars="condition")
  colnames(melted_val) <- c("condition","Name","value")
  melted_val <- merge(melted_val,HG18_coord, by="Name")
  return(melted_val)
  
}