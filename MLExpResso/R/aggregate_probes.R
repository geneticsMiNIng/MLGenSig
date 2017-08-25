#' @title Aggregating CpG probes to genes
#'
#' @description Function \code{aggregate_probes} aggregates CpG probes to corresponding genes.
#'
#' @param data data frame containing methylation values for CpG probes. Columns correspond to probes, rows to samples.
#' @param keep the name of the column or vector of columns names we want to keep.
#' @param genom.data data frame which contains information about CpG probes and corresponding genes, by default in our package we use \code{\link{illumina_humanmethylation_27_data}}.
#' @param genes.col number of column in genom.data containing informations about genes (genes symbols).
#' @param probes.col number of column in genom.data containing informations about probes (probes symbols).
#'
#' @return A data frame with CpG probes aggregated to genes. If there were more than one probe corresponding to a gene, value is a mean of those probes.
#'
#' @examples
#'
#' \dontrun{
#' library(MLExpRessoData)
#' BRCA_genes <- aggregate_probes(BRCA_methylation_all, keep="SUBTYPE")
#' }
#'
#' @export

aggregate_probes <- function(data, keep=NULL, genom.data= MLExpResso::illumina_humanmethylation_27_data, genes.col=11, probes.col=1) {
  if (!is.null(keep)) {
    if (length(keep) == 1) {
      keep_col <- as.data.frame(data[, keep])
      colnames(keep_col) <- keep
    } else {
      keep_col <- data[, which(colnames(data) %in% keep)]
    }
  }

  dict_cg <- as.vector(genom.data[, genes.col])
  names(dict_cg) <- genom.data[, probes.col]
  rnames <- rownames(data)

  colnames(data) <- dict_cg[colnames(data)]
  data <- as.data.frame(lapply(split(as.list(data), f = colnames(data)), function(x) Reduce(`+`, x) / length(x)))
  rownames(data) <- rnames
  if (!is.null(keep)) {
    data <- cbind(keep_col, data)
  }
  return(data)
}
