#' @title Visulisations for methylation and expression.
#'
#' @description Function \code{plot_volcanoes} generate a dashboard with volcano plots for expression and methylation. Also it adds a tables with basic statistics.
#'
#' @param data.m Data frame containing information for methylation.
#' @param data.e Data frame containing information for expression.
#' @param condition.m Condition for methylation data (data.m).
#' @param condition.e Condition for expression methylation (data.e).
#' @param gene Gene name.
#' @param test.m List of tests results for methylation.
#' @param test.e List of tests results for expression
#' @param values Logical. If TRUE p-values and log fold for chosen gene will be added to a plot. By default we use FALSE.
#' @param font.size Font size in stats table.
#'
#' @return An object of class ggplot containing volcano plots of p-values versus log2.fold for gene for chosen number of tests. Also there are added simple statistisc about chosen gene.
#'
#' @importFrom gridExtra tableGrob
#' @importFrom grid textGrob
#' @importFrom grid gpar
#' @importFrom gridExtra grid.arrange
#' @importFrom edgeR cpm
#' @importFrom grid grid.rect
#'
#' @seealso \code{\link{plot_volcano}}
#'
#' @examples
#' \dontrun{
#' library(MLExpRessoData)
#' cond_exp <- ifelse(BRCA_exp$SUBTYPE == "LumA","LumA","other")
#' cond_met <- ifelse(BRCA_met$SUBTYPE == "LumA","LumA","other")
#'
#' BRCA_met_gen <- aggregate_probes(BRCA_met[,-1])
#'
#' test1  <- calculate_test(BRCA_exp[,-1], condition_exp, test="lrt")
#' test2  <- calculate_test(BRCA_met_gen, condition_met, test="ttest")
#'
#' plot_volcanoes(BRCA_met[,-1],BRCA_exp[,-1],cond_met, cond_exp, "ICAM2", test1, test2, values=TRUE)
#' }
#'
#' @seealso \code{\link{plot_volcano}}
#'
#' @export

plot_volcanoes <- function(data.m, data.e, condition.m, condition.e, gene=NA, test.m=list(), test.e=list(), values=FALSE, font.size=6) {
  if (class(test.e) != "list") {
    test.e <- list(test.e)
  }
  if (class(test.m) != "list") {
    test.m <- list(test.m)
  }

  names.e <- names(test.e)
  names.m <- names(test.m)
  if (length(names.e) == 0) {
    names.e <- rep("", times = length(test.e))
  }
  if (length(names.m) == 0) {
    names.m <- rep("", times = length(test.m))
  }
  mytheme <- gridExtra::ttheme_default(
    core = list(fg_params = list(cex = 1.8, col = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#ffff33", "#a65628", "#f781bf")), bg_params = list(fill = c("white"))),
    colhead = list(fg_params = list(cex = 1.8), bg_params = list(fill = c("white"))),
    rowhead = list(fg_params = list(cex = 1.8, fontface = "bold")),
    base_size = font.size)

  if (!is.na(gene)) {
    data.e.cpm <- as.data.frame(cpm(data.e))
    s.e <- tableGrob(t(gene_stats(data.e.cpm, condition.e, gene, 0)), theme = mytheme)
    data.m.map <- aggregate_probes(data.m)
    s.m <- tableGrob(t(gene_stats(data.m.map, condition.m, gene, 2)), theme = mytheme)

    title.e <- textGrob("Expression (cpm)", just = "centre", gp = gpar(fontsize = 25))
    title.m <- textGrob("Methylation", just = "centre", gp = gpar(fontsize = 25))


    title <- textGrob(gene, gp = gpar(fontsize = 25), x = unit(0, "lines"), hjust = 0, vjust = 0)
    blank <- textGrob("", gp = gpar(fontsize = 25))

    plist <- list(title, blank, title.m, title.e, s.m, s.e)
  } else {
    plist <- list()
  }
  if ((length(test.e) + length(test.m)) > 0) {
    l.e <- length(test.e)
    l.m <- length(test.m)

    for (i in 1:max(l.e, l.m)) {
      if (i > l.m) {
        plist[[length(plist) + 1]] <- grid.rect(gp = gpar(col = "white"))
      }
      else {
        plist[[length(plist) + 1]] <- plot_volcano(test.m[[i]], ngen = gene, title = names.m[i], ylog = TRUE, line = 0.05, values = values)
      }

      if (i > l.e) {
        plist[[length(plist) + 1]] <- grid.rect(gp = gpar(col = "white"))
      } else {
        plist[[length(plist) + 1]] <- plot_volcano(test.e[[i]], ngen = gene, ylog = TRUE, title = names.e[i], line = 0.05, values = values)
      }
    }
  }
  heights.plots <- rep(130, max(length(test.e), length(test.m)))
  if (!is.na(gene)) {
    heights.g <- unit(c(20, 10, 30, heights.plots), "mm")
    grid.arrange(grobs = plist, ncol = 2, heights = heights.g)
  } else {
    grid.arrange(grobs = plist, ncol = 2, heights = heights.plots)
  }
}
