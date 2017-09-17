#devtools::install_github("geneticsMiNIng/MLGenSigdata/MLExpRessodata")
#devtools::install_github("geneticsMiNIng/MLGenSig/MLExpResso")

library(MLExpResso)

##### 1) Identification of genes with affected expression #####

# 1.1) Data set
#   Load data sets containing information about gene expression: read counts per-gene, computed for patients with lung cancer.
#   Rows of this data set correspond to samples taken from patients (first column contains sample ID).
#   Second column `survival_status` takes the value 1 for patients which lived more than six months after cancer recognition, 0 otherwise. 
#   Next columns correspond to genes.

load(url("https://github.com/geneticsMiNIng/MLGenSigdata/blob/master/MLExpRessodata/data/LUSC_exp.rda?raw=true"))
head(LUSC_exp)[1:5, 1:5]


# 1.2) Condition
#   Create a variable containing a vector with survival status for every patient.





# 1.3) Tests
#   Use calculate_test() function to test for differencies in genes expression for patients with different survival status.
#   You can use one of 3 tests: negative binomial (nbinom2), Likelihood-ratio test (lrt), Quasi-likelihood F-test (qlf).





# 1.4) Visualize results
#   To visualize reults you can use plot_volcano() function.
#   Remember that you can use the additional arguments in plot_volcano(). See ?plot_volcano




##### 2) Identification of DMR - differentially methylated regions #####

# 2.1) Data set
#   Load data set containing information about methylation of CpG probes for patients with lung cancer.
#   Rows of this data set correspond to samples taken from patients.
#   Second column `survival_status` takes the value 1 for patients which lived more than six months after cancer recognition, 0 otherwise. 
#   Values inside the table indicate the percentage methylation level of CpG probe for specified sample.

load(url("https://github.com/geneticsMiNIng/MLGenSigdata/blob/master/MLExpRessodata/data/LUSC_met.rda?raw=true"))
head(LUSC_met)[1:5,1:5]

# 2.2) Condition
#   Create a variable containing a vector with survival status for every patient.



# 2.3) Aggregation to genes
#   Aggregate CpG probes to genes using aggregate_probes() function.
#   Remember about `keep` argument to don't drop columns `id` and `survival_status`



# 2.4) Testing
#   Use calculate_test() function to test for differencies in DNA methylation level in patients with different survival statuses.
#   You can use t-student test by ttest value of agument `test`.




# 2.5) Visualization
#   To visualize reults you can use plot_volcano() and plot_methylation_path() functions.
#   Remember that you can use the additional arguments in plot_volcano(). See ?plot_volcano
#   Remember that you can use the additional arguments in plot_methylation_path(). See ?plot_methylation_path






###### 3) Identification of regions with changes in expression and methylation #####

# 3.1) Find genes with differences both in expression and methylation.
#   You can use calculate_comparison_table() function.






# 3.2) Visualize identified genes. You can use plot_volcanoes() and plot_gene() functions.


