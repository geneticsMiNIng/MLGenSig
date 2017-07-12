# MLGenSig
Machine Learning for Genetic Signatures

## Imports installation
Our package uses a few packages from Bioconductor. To install them, start R and enter

```
## try http:// if https:// URLs are not supported
source("https://bioconductor.org/biocLite.R")
biocLite("DESeq")

## try http:// if https:// URLs are not supported
source("https://bioconductor.org/biocLite.R")
biocLite("DESeq2")

## try http:// if https:// URLs are not supported
source("https://bioconductor.org/biocLite.R")
biocLite("limma")

## try http:// if https:// URLs are not supported
source("https://bioconductor.org/biocLite.R")
biocLite("methyAnalysis")

## try http:// if https:// URLs are not supported
source("https://bioconductor.org/biocLite.R")
biocLite("TxDb.Hsapiens.UCSC.hg18.knownGene")

## try http:// if https:// URLs are not supported
source("https://bioconductor.org/biocLite.R")
biocLite("`org.Hs.eg.db")
```


## Installation 
To install this package, start R and enter:
```
devtools::install_github("geneticsMiNIng/MLGenSig/MLExpResso")
```


## Vignettes

* [Usage](https://github.com/geneticsMiNIng/MLGenSig/blob/master/Vignette/Usage.pdf)
* [Automatically created report](https://github.com/geneticsMiNIng/MLGenSig/blob/master/Reports/plots.pdf) and [Instruction](https://github.com/geneticsMiNIng/MLGenSig/blob/master/Reports/generating_reports.pdf)



## Scripts and datasets

* [Full BRCA mRNAseq data](https://github.com/geneticsMiNIng/MLGenSig/blob/master/Scripts/BRCA_mRNAseq/downloadBRCA.R)
* [Full BRCA mRNAseq data with subtypes](https://github.com/geneticsMiNIng/MLGenSig/tree/master/Scripts/BRCA_mRNAseq_with_SUBTYPES)
* [BRCA clinical data with parameters](https://raw.githubusercontent.com/geneticsMiNIng/MLGenSig/master/SubTypes/BRCA_clinical_parameters.csv)
* [BRCA clinical data w/o parameters](https://raw.githubusercontent.com/geneticsMiNIng/MLGenSig/master/SubTypes/BRCA_clinical_2.csv)


## Example dashboard

![Dashboard](https://github.com/geneticsMiNIng/MLGenSig/blob/master/Images/CACNA1G.png)
