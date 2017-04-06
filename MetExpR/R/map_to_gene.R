#' @title mapping CpG islands to genes
#'
#' @description Function \code{map_to_gene} maps CpG islands to corresponding genes.
#'
#' @param data data frame ... Columns corresponds to genes, rows to samples.
#' @param illumina_humanmethylation_27_data dataset containing informations about CpG islands
#'
#' @return A data frame with CpG islands mapped to genes. If there were more than one island corresponding to a gene, 
#' value is a mean of those islands.
#'   
#'
#' @export
 
map_to_gene <- function(data,illumina_humanmethylation_27_data){
  genom <- illumina_humanmethylation_27_data[,c(1,11)]
  dict_cg <- as.vector(genom$Symbol)
  names(dict_cg) <- genom$Name
  rnames<-rownames(data)
  colnames(data) <- dict_cg[colnames(data)]
  data<-as.data.frame(lapply(split(as.list(data),f = colnames(data)),function(x) Reduce(`+`,x) / length(x)))
  rownames(data) <- rnames
  return(data)
} 
