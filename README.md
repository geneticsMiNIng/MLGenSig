[![Build status](https://travis-ci.org/geneticsMiNIng/MLGenSig.svg?branch=master)](https://travis-ci.org/geneticsMiNIng/MLGenSig)


# MLGenSig
Machine Learning for Genetic Signatures

# MLExpResso
Package for analysing genes expression and CpG islands metylation.

### [News](https://github.com/geneticsMiNIng/MLGenSig/blob/master/NEWS.md)

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

In order to run examples you shall install [`MLGenSigdata`](https://github.com/geneticsMiNIng/MLGenSigdata).

```
devtools::install_github("geneticsMiNIng/MLGenSigdata/MLExpRessodata")
```


## User Guide

* [Quick start](https://github.com/geneticsMiNIng/MLGenSig/blob/master/QuickStart/QuickStart.pdf)
* [Automatically created report](https://github.com/geneticsMiNIng/MLGenSig/blob/master/Reports/plots.pdf) and [Instruction](https://github.com/geneticsMiNIng/MLGenSig/blob/master/Reports/generating_reports.pdf)
* Cheatsheet

## Scripts and datasets

* [Full BRCA mRNAseq data](https://github.com/geneticsMiNIng/MLGenSig/blob/master/Scripts/BRCA_mRNAseq/downloadBRCA.R)
* [Full BRCA mRNAseq data with subtypes](https://github.com/geneticsMiNIng/MLGenSig/tree/master/Scripts/BRCA_mRNAseq_with_SUBTYPES)
* [BRCA clinical data with parameters](https://raw.githubusercontent.com/geneticsMiNIng/MLGenSig/master/SubTypes/BRCA_clinical_parameters.csv)
* [BRCA clinical data w/o parameters](https://raw.githubusercontent.com/geneticsMiNIng/MLGenSig/master/SubTypes/BRCA_clinical_2.csv)


## Example dashboards
![plot_volcanoes](https://github.com/geneticsMiNIng/MLGenSig/blob/master/Images/plot_volcanoes_CACNA1G.png)
![plot_gene](https://github.com/geneticsMiNIng/MLGenSig/blob/master/Images/plot_gene_CACNA1G.png)
