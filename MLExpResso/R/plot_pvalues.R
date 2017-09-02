#' @title Visualise the p-values of expression and methylation for genes.
#'
#' @description Function \code{plot_pvalues} draws a plot with p-values from 2 tests for methylation and expression.
#'
#' @param data Data.frame - result of `calculate_comparison_table()` function for methylation and expression data.
#' @param names Number of genes to be labeled. Genes are selected based on the ranking of the most significant changed genes in terms of both methylation and expression - geom.mean.rank column. More: \code{\link[MLExpResso]{calculate_comparison_table}}.
#'
#' @return An object of class ggplot containing a plot of p-values.
#'
#' @importFrom dplyr arrange
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggrepel geom_text_repel
#' @importFrom grid unit
#'
#' @seealso calculate_comparison_table
#'
#' @export

plot_pvalues <- function(data, names=NA) {
  id <- geom.mean.rank <- NULL
  tests <- colnames(data)[c(3, 5)]
  plot <- ggplot(data, aes(x = -log(data[ ,3]), y = -log(data[ ,5]))) +
    geom_point() +
    theme_bw() +
    ggtitle("P-values comparison") +
    xlab(paste0("- Log of ", tests[1])) +
    ylab(paste0("- Log of ", tests[2]))
  if (!is.na(names)) {
    rank <- head(data.frame(arrange(data, geom.mean.rank)), names)
    plot <- plot + geom_text_repel(
      data = rank,
      aes(x = -log(rank[ ,3]), y = -log(rank[ ,5]), label = id),
      size = 3,
      box.padding = unit(0.35, "lines"),
      point.padding = unit(0.3, "lines")
    )
  }
  return(plot)
}
