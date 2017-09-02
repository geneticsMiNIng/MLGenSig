#' @title Visualise the p-values for genes.
#'
#' @description Function \code{plot_means} draws a plot with p-values from methylation when we use the t-test. We testing the equality of means in given two groups.
#'
#' @param data Data set with information from methylation or expression. See: \code{\link[MLExpResso]{calculate_test}}.
#' @param condition A factor of levels corresponding to order of samples in data.
#' @param names number of genes to be labeled. On a plot are marked genes with the smallest geometric mean of means.
#'
#' @return a plot of p-values
#'
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_abline
#' @importFrom ggrepel geom_text_repel
#' @importFrom grid unit
#' @importFrom utils head
#' @importFrom utils tail
#'
#' @seealso p_values_plot
#'
#' @export

plot_means <- function(data, condition, names=NA) {
  id <- NULL
  dataA <- data[which(condition == unique(condition)[1]), ]
  dataB <- data[which(condition == unique(condition)[2]), ]
  meanA <- sapply(dataA, mean)
  meanB <- sapply(dataB, mean)
  data <- as.data.frame(meanA)
  data$id <- rownames(data)
  data$meanB <- meanB
  plot <- ggplot(data, aes(x = meanA, y = meanB)) +
    geom_point() +
    geom_abline(slope = 1) +
    theme_bw() +
    ggtitle("Means in groups") +
    xlab(paste0("Group ", unique(condition)[1])) +
    ylab(paste0("Group ", unique(condition)[2]))+
    scale_x_continuous(labels = function(x) format(x, scientific = FALSE))
  if (!is.na(names)) plot <- plot + geom_text_repel(
    data = tail(data[order(data$meanB - data$meanA), ], names),
    aes(label = id),
    size = 3,
    box.padding = unit(0.35, "lines"),
    point.padding = unit(0.3, "lines")
  )

  return(plot)
}
