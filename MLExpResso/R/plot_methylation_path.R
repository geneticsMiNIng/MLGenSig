#' @title Methylation path on chosen gene.
#'
#' @description Function \code{plot_methylation_path} visualizes a chosen gene with marked CpG probes. Y axis describes methylation level. X axis describes a location of the probe on the chromosome. Horizontal lines show the mean methylation level for each Island in a division to groups. Groups are defined by colors. Large dots symbolize means of methylation level for CpG probes, small dots symbolize methylation levels for each observation. Also, we can exact the line corresponding to a gene. In this case, we see what are the locations of probes on a gene in HG18 coordinates. We can as well draw locations of CpG islands.
#'
#' @param data Data frame containing values from methylation: columns corespond to CpG probes, rows to samples.
#' @param condition Vector of levels corresponding to order of samples in data.
#' @param gene Name of chosen gene.
#' @param show.gene Logical. If TRUE arrows corresopnding to gene will be drawn.
#' @param observ Logical. If TRUE dots corresponding to CpG probes will be drawn.
#' @param islands Logical. If TRUE line corresopnding to islands should be drawn.
#' @param title Logical. If TRUE title saying what gene we visualise will be added.
#' @param genom.data Data frame which contains information about CpG probes and corresponding genes, by default in our package we use \code{\link{illumina_humanmethylation_27_data}}. New dataset should contain 5 columns with:
#' 1) CpG probe names, 2) CpG probe locations, 3) gene names, 4) logical value if there is a CpG island, 5) location of island.
#' @param mean.size size of mean dots
#' @param observ.size size of observation dots
#' @param ... Other parameters.
#'
#' @return Object of class ggplot containing visualisation of methylation on gene.
#'
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 geom_segment
#' @importFrom ggplot2 ylim
#' @importFrom grid arrow
#' @importFrom scales percent
#'
#' @examples
#' \dontrun{
#' library(MLExpRessoData)
#' condition_met <- ifelse(BRCA_met$SUBTYPE == "LumA","LumA", "other")
#' plot_methylation_path(BRCA_met, condition_met, "BRCA2")
#' }
#'
#' @seealso \code{\link{plot_gene}}
#'
#' @export


plot_methylation_path <- function(data, condition, gene, show.gene=FALSE,
                                  observ=FALSE, islands = TRUE, title=TRUE, genom.data = NULL,
                                  mean.size=4, observ.size = 0.5, ...) {
  HG18_coord <- value <- island_cond <- NULL

  genom.data <- make_dictionary_data(genom.data)
  err_plot_methylation_path(data, genom.data)


  groups <- unique(condition)
  CpG_means <- list()
  data2 <- data.frame(Name=character(), HG18_coord=character(), Symbol=character(),
                      CPG_ISLAND=logical(), CPG_ISLAND_LOCATIONS=character(),
                      mean=numeric(), condition = character())

  for(group in groups){
    group_data <- data[which(condition == group), ]
    CpG_mean <- calculate_CpG_mean(group_data, gene, genom.data = genom.data)
    CpG_mean$condition <- group
    data2 <- rbind(data2, CpG_mean)
  }

  data2$Name_loc <- paste(data2$HG18_coord, "\n", data2$Name)
  plot1 <- ggplot(data2, aes(HG18_coord, mean, group = condition, colour = condition)) +
    theme_bw() +
    xlab(paste0("Gene ", gene)) +
    theme(legend.position = "top",
      axis.title.x = element_blank(),
      axis.text.x = element_text(size = 15, angle = 0, hjust = 0.5),
      axis.text.y = element_text(size = 15),
      panel.border = element_blank()) +
    scale_y_continuous(limits = c(-0.05, 1), expand = c(0.05, -0.05), labels = percent) +
    scale_color_manual(values = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#ffff33", "#a65628", "#f781bf")) +
    ylab("")
  if (observ == TRUE) {
    observations_coord <- probes_locations(data, gene, condition, genom.data = genom.data)
    plot1 <- plot1 + geom_point(data = observations_coord, aes(HG18_coord, value), size = observ.size, alpha = 0.35)
  }

  # Means over observations
  plot1 <- plot1 + geom_point(size = mean.size)

  gene_loc <- gene_location(gene)

  if (show.gene == TRUE) {
    if (gene_loc[2] < min(data2$HG18_coord)) {
      plot1 <- plot1 + geom_segment(aes(x = gene_loc[1], xend = gene_loc[2], y = -0.025, yend = -0.025), colour = "blue", size = 1, arrow = arrow(length = unit(0.3, "cm"), ends = "first"))
    }
    if (gene_loc[2] > max(data2$HG18_coord)) {
      plot1 <- plot1 + geom_segment(aes(x = max(gene_loc[1], min(data2$HG18_coord)), xend = min(gene_loc[2], max(data2$HG18_coord)) + 1000, y = -0.025, yend = -0.025), colour = "blue", size = 1, arrow = arrow(length = unit(0.3, "cm"), ends = "last"))
    }
    if ((gene_loc[1] > min(data2$HG18_coord)) && (gene_loc[2] > max(data2$HG18_coord))) {
      plot1 <- plot1 + geom_segment(aes(x = max(gene_loc[1], min(data2$HG18_coord)), xend = min(gene_loc[2], max(data2$HG18_coord)), y = -0.025, yend = -0.025), colour = "blue", size = 1)
    }
  }

  if (islands == TRUE) {
    data_islands <- islands_locations(data2)
    plot1 <- plot1 + geom_segment(data=data_islands, aes(y = mean, yend = mean,
                                                           x = as.numeric(START), xend = as.numeric(END)))  }
  if (title == TRUE) {
    plot1 <- plot1 + ggtitle(paste(gene))
  }

  return(plot1)
}
