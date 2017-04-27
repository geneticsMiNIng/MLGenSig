#' @title gene_loc
#'
#' @description Function \code{gene_loc} ...
#'
#' @param gen s
#'
#' @return loacation of gene.
#' @importFrom AnnotationDbi select
#' @importFrom org.Hs.eg.db org.Hs.eg.db
#' @importFrom TxDb.Hsapiens.UCSC.hg18.knownGene TxDb.Hsapiens.UCSC.hg18.knownGene
#'
#' @export

gene_loc <-function(gene){

  a <- AnnotationDbi::select(org.Hs.eg.db,
                             keys = gene,
                             columns=c("ENTREZID", "SYMBOL"),
                             keytype="SYMBOL")
  b <- AnnotationDbi::select(TxDb.Hsapiens.UCSC.hg18.knownGene,
                             keys = a$ENTREZID,
                             columns=c('GENEID', 'TXCHROM', 'TXSTART', 'TXEND'),
                             keytype="GENEID")
  names(b) <- c('ENTREZID', 'TXCHROM', 'TXSTART', 'TXEND')
  c <- merge(a, b, 'ENTREZID')
  c
  
  data_gene_position <- c
  gene_loc <- c(min(data_gene_position$TXSTART),max(data_gene_position$TXEND))
  return(gene_loc)

}
