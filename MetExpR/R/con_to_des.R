#' @title Function transforms 'condition' data format to 'design' data format
#'
#' @description a
#'
#' @param condition vector of level values
#'
#' @return data frame of zeros and ones with rows coresponding to sample and columns coresponding to level

con_to_des <- function(condition){
  levels <- unique(condition)
  design <- data.frame(c1 = ifelse(condition==levels[1],1,0),
                       c2 = ifelse(condition==levels[2],1,0))
  colnames(design) <- levels
  return(design)
}
