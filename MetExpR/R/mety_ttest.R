#' @title mety_test: t test for metylation
#'
#' @description Function \code{mety_test} computes statistics and p-values for metylation data
#'
#' @param data Strategia, przy ktorej ma byc badana gra
#' @param condition Ile razy fukcja ma zagrac w SuperFarmera przy zadanej strategii, domyslnie 10000
#'
#' @return
#'
#' @export


mety_ttest <-function(data, condition){
  design <- con_to_des(condition)
  fit <- lmFit(data, design)
  form <- paste0(colnames(design)[1],"-",colnames(design)[2])
  contMatrix <- makeContrasts(form, levels=design)
  fit2 <- contrasts.fit(fit, contMatrix)
  fit2 <- eBayes(fit2)
  DMPs <- topTable(fit2, num=Inf, coef=1)
  return(DMPs)
}
