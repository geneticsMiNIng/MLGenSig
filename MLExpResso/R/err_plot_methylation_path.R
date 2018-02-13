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
#' @keywords internal


err_plot_methylation_path <- function(data, genom.data = NULL) {
  genom.data = make_dictionary_data(genom.data)
  if (!any(colnames(data) %in% genom.data[, 1])) stop("Wrong colnames, check if a data set contains not aggregated values.")
  return(NULL)
}
