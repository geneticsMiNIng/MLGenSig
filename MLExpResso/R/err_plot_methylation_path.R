#' @title Correct Data
#'
#' @description Function checks if data is correct - not aggregated to genes.
#'
#' @param data data frame containing values from methylation: columns corespond to CpG probes, rows to samples.
#' @param genom.data data frame which contains information about CpG probes and corresponding genes, by default in our package we use \code{\link{illumina_humanmethylation_27_data}}.
#' @param probes.col number of column in genom.data containing informations about probes (probes symbols).
#
#' @return Error or nothing
#'



err_plot_methylation_path <-function(data, genom.data = MLExpResso::illumina_humanmethylation_27_data, probes.col=1){
  if( !(any(colnames(data) %in% genom.data[,1])) ) stop("Wrong colnames, check if a data set contains not aggregated values.")
  return(NULL)
}
