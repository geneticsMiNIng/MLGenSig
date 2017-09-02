#' @title Visualise the p-values of expression and methylation for genes.
#'
#' @description Function \code{plot_volcano} draws a plot with p-values and fold logarithm from methylation or expression when we use the t-test.
#'
#'
#' @param data Data frame containing result of chosen test from \code{\link{calculate_test}} function.
#' @param line P-value on which we draw a horizontal line.
#' @param names P-value below which we want to draw genes names.
#' @param ngen Character symbol or vector of gene names to be labeled.
#' @param fold_line Value on which we want to draw a vertical line on both sides of zero.
#' @param title Character containing title for plot.
#' @param ylog Logical. If TRUE values on y-axis will be logarithmized.
#' @param values Logical. If TRUE p-values and log fold for chosen gene will be add to a plot. By default we use FALSE.
#'
#' @return An object of class ggplot containing volcano plot of p-values versus log2.fold for each gene.
#'
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 scale_color_manual
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_hline
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggplot2 geom_vline
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 annotate
#' @importFrom ggplot2 ggplot_build
#' @importFrom grid unit
#' @importFrom scales trans_breaks
#' @importFrom scales trans_format
#' @importFrom ggthemes extended_range_breaks
#' @importFrom scales math_format
#' @importFrom scales trans_new
#' @importFrom stringr str_sub
#' @importFrom stringr str_length
#'
#' @examples
#' \dontrun{
#' library(MLExpRessoData)
#' plot_volcano(BRCA_met, values=TRUE)
#' }
#'
#' @seealso \code{\link{plot_volcanoes}}
#' @export

plot_volcano <- function(data, line=NA, names= NA, ylog=TRUE, ngen=NA, title=NA, fold_line=NA, values=FALSE) {
  .x <- NULL

  log2.fold <- pval <- id <- NULL
  colnames(data) <- ifelse(str_sub(colnames(data), str_length(colnames(data)) - 3, str_length(colnames(data))) == "pval", "pval", colnames(data))
  colnames(data) <- ifelse(str_sub(colnames(data), str_length(colnames(data)) - 8, str_length(colnames(data))) == "log2.fold", "log2.fold", colnames(data))


  if (ylog == TRUE) {
    data$pval <- data$pval
  }
  plot <- ggplot(data, aes(log2.fold, pval)) +
    geom_point(size = 0.5) +
    theme_bw(base_size = 12) +
    theme(
      panel.border = element_blank(),
      axis.text.x = element_text(size = 15),
      axis.text.y = element_text(size = 15)
    ) +
    scale_y_continuous(
      trans = reverselog_trans(10),
      breaks = trans_breaks("log10", function(x) 10 ^ x),
      labels = trans_format("identity", math_format(10 ^ .x))
    ) +
    scale_x_continuous(
      breaks = extended_range_breaks()(data$log2.fold),
      labels = function(x) sprintf("%.2f", x)
    )

  if (is.na(title)) {
    plot <- plot + ggtitle("")
  } else {
    plot <- plot + ggtitle(paste0(title))
  }
  if (!is.na(fold_line)) {
    plot <- plot + geom_vline(xintercept = c(-fold_line, fold_line), col = "red")
  }
  if (!is.na(line)) {
    if (ylog == TRUE) {
      plot <- plot + geom_hline(yintercept = line, col = "red") + ylab("-log10(pval)")
    } else {
      plot <- plot + geom_hline(yintercept = 10 ^ (line), col = "red")
    }
  }
  if (!is.na(names) & names < 1) plot <- plot + geom_text_repel(
    data = subset(data, pval < names),
    aes(label = id),
    size = 3,
    col = "grey",
    box.padding = unit(0.35, "lines"),
    point.padding = unit(0.3, "lines")
  )
  if (!is.na(names) & names >= 1) plot <- plot + geom_text_repel(
    data = head(data[order(data$pval), ], names),
    aes(label = id),
    size = 3,
    box.padding = unit(0.35, "lines"),
    point.padding = unit(0.3, "lines")
  )
  if (!is.na(ngen)) {
    data2 <- data[which(data$id %in% ngen), ]
    plot <- plot +
      geom_point(data = data2, aes(log2.fold, pval), col = "red", size = 2)
    if (length(ngen) > 1) {
      plot <- plot +
        geom_text_repel(
          data = data[which(data$id %in% ngen), ],
          aes(label = id),
          size = 3,
          col = "grey",
          box.padding = unit(0.35, "lines"),
          point.padding = unit(0.3, "lines")
        )
    }
  }

  if (values == TRUE && !is.na(ngen)) {
    values_for_gene <- data[which(data$id == ngen), ]
    breaks_y <- ggplot_build(plot)$layout$panel_ranges[[1]]$y.major_source
    diff_y <- (breaks_y[1] - breaks_y[2]) / 5
    breaks_x <- ggplot_build(plot)$layout$panel_ranges[[1]]$x.major_source
    len <- length(breaks_x)
    diff_x <- breaks_x[len]
    label_pvalue <- NULL
    if (values_for_gene$pval < 10 ^ (-4)) {
      label_pvalue <- paste("pval < 0.0001")
    } else {
      label_pvalue <- paste("pval:", round(values_for_gene$pval, 4))
    }

    plot <- plot +
      annotate(
        "text",
        x = 0,
        y = (min(data$pval) + 10 ^ (-breaks_y[1])),
        label = label_pvalue, colour = "red"
      ) +
      annotate(
        "text",
        x = 0,
        y = (min(data$pval) + 10 ^ (-breaks_y[1] + diff_y)),
        label = paste("log2.fold:", round(values_for_gene$log2.fold, 4)),
        colour = "red"
      )
  }


  return(plot)
}
