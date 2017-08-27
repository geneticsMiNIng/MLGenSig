#' @title design_format
#'
#' @description Function \code{design_format} transforms 'condition' data format to 'design' data format. It transform vector of levels corresponding to samples.
#'
#' @param condition vector of levels values
#'
#' @return data frame of zeros and ones with rows coresponding to sample and columns coresponding to level.
#' @keywords internal

con_to_des <- function(condition) {
  levels <- unique(condition)
  design <- data.frame(c1 = ifelse(condition == levels[1], 1, 0),
    c2 = ifelse(condition == levels[2], 1, 0))
  colnames(design) <- levels
  return(design)
}
