#' @title Boxplot for expression in groups
#'
#' @description Function \code{plot_diff_boxplot} generates a boxplot of values from choosen data frame column with division in groups.
#'
#' @param data Data frame containing interesing values.
#' @param condition Vector of length equal to numer of rows of data. Vector \code{condition} should contains names of groups corresponding to rows.
#' @param gene String containing name of gene with values for boxplot.
#' @param sqrt.trans Root square y-axis transformation.
#' @param title Plot title, by default id of gene.
#'
#' @return Object of class ggplot containing boxplot for values from choosen gene broken down to groups.
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_boxplot
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 coord_trans
#' @importFrom ggplot2 ggtitle
#' @importFrom edgeR cpm
#'
#' @examples
#' \dontrun{
#' library(MLExpRessoData)
#' condition_expr <- ifelse(BRCA_exp7$SUBTYPE == "LumA","LumA","other")
#' plot_diff_boxplot(BRCA_exp, "BRCA2", condition_expr, sqrt.trans=TRUE)
#' }
#'
#' @seealso \code{\link{plot_gene}}
#' @export

plot_diff_boxplot <- function(data, condition="", gene, sqrt.trans=FALSE, title=TRUE) {
  values <- NULL

  data <- data[, gene]
  data <- cpm(data)

  if (is.numeric(data)) {
    dt <- as.data.frame(data)
    colnames(dt) <- gene
  } else {
    dt <- data[, gene]
    dt <- as.data.frame(dt)
  }
  dt$condition <- condition
  colnames(dt)[1] <- "values"
  dt$gene <- paste0(gene)
  colnames(dt)[2] <- paste0(gene)

  plot <- ggplot(dt, aes(condition, y = values, col = condition)) +
    geom_boxplot(outlier.size = 0.5) +
    theme_bw() +
    theme(axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_text(size = 15, angle = 0, hjust = 0.5),
      axis.text.y = element_text(size = 15),
      legend.position = "none",
      panel.border = element_blank()) +
    xlab(condition) +
    scale_color_manual(values = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#ffff33", "#a65628", "#f781bf")
    )

  if (sqrt.trans == TRUE) plot <- plot + coord_trans(y = "sqrt")
  if (title == TRUE) {
    plot <- plot + ggtitle(gene)
  }

  return(plot)
}
