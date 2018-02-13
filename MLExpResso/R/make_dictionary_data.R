#' @title transforms genome dataset
#'
#' @description Trensforms new genome dataset into the form used by functions in MLExpResso package.
#'
#' @param genom.data new genome datset
#'
make_dictionary_data <- function(genom.data=NULL){

  if(is.null(genom.data)) {
    genom.data <- MLExpResso::illumina_humanmethylation_27_data[,c(1,4,11,18,19)]
  } else{
    colnames(genom.data) <- c("Name", "HG18_coord", "Symbol", "CPG_ISLAND", "CPG_ISLAND_LOCATIONS")
  }
  return(genom.data)
}
