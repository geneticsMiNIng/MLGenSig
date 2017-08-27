#' @title Genes location
#'
#' @description Function \code{gene_location} ...
#'
#' @param gene symbol of gene location we want to get.
#'
#' @return vector containing the beging and ending of gene.
#' @importFrom AnnotationDbi select
#' @importFrom org.Hs.eg.db org.Hs.eg.db
#' @importFrom TxDb.Hsapiens.UCSC.hg18.knownGene TxDb.Hsapiens.UCSC.hg18.knownGene
#'
#' @keywords internal

gene_location <- function(gene) {
  hs.data <- AnnotationDbi::select(org.Hs.eg.db,
    keys = gene,
    columns = c("ENTREZID", "SYMBOL"),
    keytype = "SYMBOL")
  hg18.data <- AnnotationDbi::select(TxDb.Hsapiens.UCSC.hg18.knownGene,
    keys = hs.data$ENTREZID,
    columns = c("GENEID", "TXCHROM", "TXSTART", "TXEND"),
    keytype = "GENEID")
  names(hg18.data) <- c("ENTREZID", "TXCHROM", "TXSTART", "TXEND")
  data_gene_position <- merge(hs.data, hg18.data, "ENTREZID")


  gene_loc <- c(min(data_gene_position$TXSTART), max(data_gene_position$TXEND))
  return(gene_loc)
}
