# MLGenSig
Machine Learning for Genetic Signatures

## Imports installation
Our package uses a few packages from Bioconductor. To install them, start R and enter
- `DESeq`
```
## try http:// if https:// URLs are not supported
source("https://bioconductor.org/biocLite.R")
biocLite("DESeq")
```

- `DESeq2`
```
## try http:// if https:// URLs are not supported
source("https://bioconductor.org/biocLite.R")
biocLite("DESeq2")
```

- `limma`
```
## try http:// if https:// URLs are not supported
source("https://bioconductor.org/biocLite.R")
biocLite("limma")
```

- `TxDb.Hsapiens.UCSC.hg18.knownGene`
```
## try http:// if https:// URLs are not supported
source("https://bioconductor.org/biocLite.R")
biocLite("TxDb.Hsapiens.UCSC.hg18.knownGene")
```

- `org.Hs.eg.db`
```
## try http:// if https:// URLs are not supported
source("https://bioconductor.org/biocLite.R")
biocLite("`org.Hs.eg.db")
```


## Installation 
To install this package, start R and enter:
```
devtools::install_github("geneticsMiNIng/MLGenSig/MetExpR")
```

## Vignette

* [Usage](https://github.com/geneticsMiNIng/MLGenSig/blob/master/Vignette/Usage.pdf)
* [Plots for 20 interesting genes](https://rawgit.com/geneticsMiNIng/MLGenSig/master/Vignette/wykresy.html)


## Scripts and datasets

* [Full BRCA mRNAseq data](https://github.com/geneticsMiNIng/MLGenSig/blob/master/Scripts/BRCA_mRNAseq/downloadBRCA.R)
* [Full BRCA mRNAseq data with subtypes](https://github.com/geneticsMiNIng/MLGenSig/tree/master/Scripts/BRCA_mRNAseq_with_SUBTYPES)
* [BRCA clinical data with parameters](https://raw.githubusercontent.com/geneticsMiNIng/MLGenSig/master/SubTypes/BRCA_clinical_parameters.csv)
* [BRCA clinical data w/o parameters](https://raw.githubusercontent.com/geneticsMiNIng/MLGenSig/master/SubTypes/BRCA_clinical_2.csv)
