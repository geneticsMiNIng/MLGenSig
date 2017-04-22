#'Illumina_humanmethylation_27_data
#'
#' The variables are as follows:
#' \itemize{
#'   \item Name. CG number from CG database (format cg########).
#'   \item GenomeBuild. Genome build.
#'   \item Chr. Chromosome on which the target locus is located.
#'   \item MapInfo. Cancer subtype.
#'   \item Source. Genomic position of C in CG dinucleotide.
#'   \item SourceVersion. Genomic position source.
#'   \item SourceSeq. Original sequence of the region covered by assay probes.
#'   \item TSS_Coordinat. Transcription start site genomic coordinate.
#'   \item Gene_Strand. Gene strand.
#'   \item Gene_ID. RefSeq identifier (GeneID).
#'   \item Symbol. Gene Symbol.
#'   \item Synonym. Gene synonyms.
#'   \item Accession. Gene Accession (this is the accession of the longest transcript).
#'   \item GID. GI ID.
#'   \item Annotation. Gene annotation from NCBI database.
#'   \item Product. Gene product description from NCBI database.
#'   \item Distance_to_TSS. Distance of CG dinucleotide to transcription start site.
#'   \item CPG_ISLAND. oolean variable denoting whether the loci is located in a CpG island (by relaxed definition) .
#'   \item CPG_ISLAND_LOCATIONS. Chromosomal location and genomic coordonates of the CpG island from NCBI database.
#'   \item MIR_CPG_ISLAND. Chromosome:start-end of upstream CPG island from a micro RNA.
#'   \item MIR_NAMES. Name of micro RNA near locus.
#' }
#'
#'@docType data
#'@name illumina_humanmethylation_27_data
#'@format data.frame
NULL
