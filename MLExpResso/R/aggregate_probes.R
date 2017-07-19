#' @title Aggregating CpG probes to genes
#'
#' @description Function \code{aggregate_probes} aggregates CpG probes to corresponding genes.
#'
#' @param data data frame  containing methylation values for CpG probes. Columns corresponds to probes, rows to samples.
#' @param keep the name of the column we want to keep
#'
#' @return A data frame with CpG probes mapped to genes. If there were more than one probe corresponding to a gene, value is a mean of those probes.
#'
#'@examples
#'\dontrun{
#'library(MLExpRessodata)
#'BRCA_genes <- aggregate_probes(BRCA_methylation_all, keep="SUBTYPE")
#'}
#' @export

aggregate_probes <- function(data, keep=NULL){
  if(!is.null(keep)){
    keep_column <- as.data.frame(data[,keep])
    colnames(keep_column) <- keep
  }
  genom <- illumina_humanmethylation_27_data[,c(1,11)]
  dict_cg <- as.vector(genom$Symbol)
  names(dict_cg) <- genom$Name
  rnames<-rownames(data)

  colnames(data) <- dict_cg[colnames(data)]
  data<-as.data.frame(lapply(split(as.list(data),f = colnames(data)),function(x) Reduce(`+`,x) / length(x)))
  rownames(data) <- rnames
  if(!is.null(keep)){
  data <- cbind(keep_column, data)
  }
  return(data)
}
