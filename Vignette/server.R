library(MetExpR)


shinyServer(function(input, output) {
  output$plot = renderPlot({
    gen <- input$wybranygen
    condition.m <-ifelse(BRCA_methylation_all$SUBTYPE=="LumA","LumA","other")
    condition.e <-ifelse(BRCA_mRNAseq_all$SUBTYPE=="LumA","LumA","other")
    visual_volc(condition.e, condition.m,BRCA_methylation_all[,-1],BRCA_mRNAseq_all[,-1],gene = gen,test.e=list(test_expr_brca),test.m=list(test_met_brca))
  })
  output$road = renderPlot({
    gen <- input$wybranygen
    condition.m <-ifelse(BRCA_methylation_all$SUBTYPE=="LumA","LumA","other")
    genereg_vs_met(BRCA_methylation_all[,-1],condition.m, gene=gen,show_gene = TRUE, observ=TRUE)
    
  })
  
})