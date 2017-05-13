# BiocInstaller::useDevel() # swiches to devel branch of Bioconductor
# source("https://bioconductor.org/biocLite.R") # downloads bioClite function
# biocLite("RTCGA") # installs a package


library(RTCGA)
library(data.table)

# datasets
# RTCGA:::availableDates() 
# RTCGA:::availableDataSets("BRCA")

downloadTCGA("BRCA", "Level_3__gene_expression", destDir = ".")

BRCAseq <- fread("gdac.broadinstitute.org_BRCA.Merge_rnaseq__illuminahiseq_rnaseq__unc_edu__Level_3__gene_expression__data.Level_3.2016012800.0.0/BRCA.rnaseq__illuminahiseq_rnaseq__unc_edu__Level_3__gene_expression__data.data.txt")
BRCAseq <- as.data.frame(BRCAseq)

ind <- which(BRCAseq[1,] %in% c("gene", "raw_counts"))
BRCAseqSelected <- BRCAseq[-1,ind]
rownames(BRCAseqSelected) <- BRCAseqSelected[,1]
BRCAseqSelected <- BRCAseqSelected[,-1]

BRCAseqSelectedTrans <- as.data.frame(t(BRCAseqSelected))
for (i in 1:ncol(BRCAseqSelectedTrans))
  BRCAseqSelectedTrans[,i] <- as.numeric(as.character(BRCAseqSelectedTrans[,i]))

BRCAseqSelectedTrans[1:5,100:105]
#save(BRCAseqSelectedTrans, file="BRCAseqSelectedTrans.Rda")

#####################
# Matching SAMPLES and cancer SUBTYPES from BRCA-clinical data
clinic <- read.csv("https://raw.githubusercontent.com/geneticsMiNIng/MLGenSig/master/SubTypes/BRCA_clinical_2.csv",header=TRUE)
dict <- clinic$SUBTYPE
names(dict) <- clinic$SAMPLE_BARCODE
BRCAseqSelectedTransSub <- BRCAseqSelectedTrans
BRCAseqSelectedTransSub$SUBTYPE <- dict[substr(rownames(BRCAseqSelectedTransSub),0,15)]
n <- ncol(BRCAseqSelectedTransSub)
BRCAseqSelectedTransSub <- BRCAseqSelectedTransSub[,c(n,1:n-1)] 
#Note that not every Sample have matched cancer subtype

