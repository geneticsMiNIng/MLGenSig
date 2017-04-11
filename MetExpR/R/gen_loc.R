#' @title genereg_vs_met2
#'
#' @description Function \code{genereg_vs_met2} ...
#'
#' @param gen s
#'
#' @return A plot of class ggplot.
#'
#'
#' @export

gen_loc <-function(gen){
  gloc <- all_genes[all_genes$hgnc_symbol == gen,]
  return(c(gloc$start_position,gloc$end_position))


}
