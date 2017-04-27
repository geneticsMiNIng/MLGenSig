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

- `limma`
```
## try http:// if https:// URLs are not supported
source("https://bioconductor.org/biocLite.R")
biocLite("limma")
```

`TxDb.Hsapiens.UCSC.hg18.knownGene`
```
## try http:// if https:// URLs are not supported
source("https://bioconductor.org/biocLite.R")
biocLite("TxDb.Hsapiens.UCSC.hg18.knownGene")
```

`org.Hs.eg.db`
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
