#' @title G
#'
#' @description Function \code{G}
#' 
#' @param data.e data for expression
#' @param data.m data for methylation
#' @param condition.e condition for  expression
#' @param condition.m condition for methylation
#' @param test.e test results for expression
#' @param test.m test results for methylation
#' @param genes gene name
#'
#' @return pdf with report consisting information about MLExpResso package
#' @examples
#'\dontrun{
#'library(MLExpRessodata)
#'condition_met <- ifelse(BRCA_methylation_chr17$SUBTYPE=="LumA","LumA","other")
#'condition_exp <- ifelse(BRCA_mRNAseq_chr17$SUBTYPE=="LumA","LumA","other")
#'
#'BRCA_methylation_gene <- aggregate_probes(BRCA_methylation_chr17, keep="SUBTYPE")
#'
#'test.nbinom  <- calculate_test(BRCA_mRNAseq_chr17[,-1], condition_exp, test="lrt")
#'test.tstudent  <- calculate_test(BRCA_methylation_gene[,-1], condition_met, test="ttest")
#'
#'report <- generate_report(BRCA_mRNAseq_chr17,BRCA_methylation_chr17, condition.exp, condition.met, test.nbinom, test.tstudent, "BRCA2")
#'}
#'
#'@seealso \code{\link{calculate_test}}, \code{\link{aggregate_probes}}
#'
#' @export

generate_report <- function(data.e, data.m, condition.e, condition.m, test.e, test.m, genes){

  directory <- getwd()

  environment <- new.env()

  environment$data.e <- data.e
  environment$data.m <- data.m

  environment$condition.e <- condition.e
  environment$condition.m <- condition.m

  environment$test.e <- test.e
  environment$test.m <- test.m

  environment$genes <- genes

  rmarkdown::render(paste0(path.package("MetExpR"), "/templates/plots.Rmd"),
                    "pdf_document", output_file = paste0(directory, "/plots.pdf"),
                    envir = environment)
}
