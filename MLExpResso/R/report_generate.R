#' @title G
#'
#' @description Function \code{G}
#'
#' @param data.m data for methylation
#' @param data.e data for expression
#' @param condition.e condition for  expression
#' @param condition.m condition for methylation
#' @param test.e test results for expression
#' @param test.m test results for methylation
#' @param genes gene name
#'
#' @return pdf
#'
#'
#' @export

report_generate <- function(data.e, data.m, condition.e, condition.m, test.e, test.m, genes){

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
