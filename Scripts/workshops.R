#devtools::install_github("geneticsMiNIng/MLGenSigdata/MLExpRessodata")
#devtools::install_github("geneticsMiNIng/MLGenSig/MLExpResso")

library(MLExpResso)
library(MLExpRessoData)

MLExpRessoData::LUSC_met
MLExpRessoData::LUSC_exp

#1. Expression
head(LUSC_exp)[1:5,1:5]

## Condition

condition.e <- LUSC_exp$survival_status

#



##Tests

###

#lusc_test_nbinom <- calculate_test(data = LUSC_exp[,-c(1,2)], condition = condition.e, test="nbinom")

lusc_test_nbinom <- calculate_test(data = LUSC_exp[,-c(1,2)], condition = condition.e, test="nbinom2")
#TBC1D21 przyjrzec sie genowi z tego testu 1 kwartyl prawie taki sam jak mediana

lusc_test_lrt <- calculate_test(data = LUSC_exp[,-c(1,2)], condition = condition.e, test="lrt")

lusc_test_qlf <- calculate_test(data = LUSC_exp[,-c(1,2)], condition = condition.e, test="qlf")
#porownac wyniki testow

plot_volcano(lusc_test_lrt)

##boxplot
plot_diff_boxplot(LUSC_exp[,-c(1,2)], condition = condition.e, gene = "PLA2G4E", sqrt.trans = TRUE)


#Methylation

head(LUSC_met)[1:5,1:5]

## Condition
condition.m <- LUSC_met$survival_status

##Aggregation to genes
LUSC_met_gen <- aggregate_probes(LUSC_met, keep = c("id","survival_status"))

##Testing

lusc_test_t <- calculate_test(LUSC_met_gen[,-c(1,2)], condition.m, test="ttest")

plot_volcano(lusc_test_t)

##Methylation path

plot_methylation_path(LUSC_met, condition.m, "PYGO1", show_gene = TRUE, observ = TRUE)

#Comparison

comparison_table <- calculate_comparison_table(LUSC_exp[,-c(1,2)],LUSC_met_gen[,-c(1,2)], condition.e, condition.m, "nbinom2","ttest")

#Dashboards
plot_volcanoes(LUSC_met[,-c(1,2)], LUSC_exp[,-c(1,2)], condition.m, condition.e, gene = "MAGEL2", test.m = comparison_table[,c(1,4,5)], test.e = comparison_table[,c(1,2,3)])

plot_gene(LUSC_met[,-c(1,2)], LUSC_exp[,-c(1,2)], condition.m, condition.e, gene = "MAGEL2")

