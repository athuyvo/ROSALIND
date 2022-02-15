# ROSALIND "COMPLEMENTING A STRAND OF DNA" solved by Anh Vo

# requires: R vesrsion 4.1 and Biostrings package

BiocManager::install("Biostrings")
library(Biostrings)

# Transform given DNA string to DNA object
# Outputs reverse complement of DNA string 
revDNA <- function(s) {
	DNAObject <- DNAString(s)
	revStrand <- reverseComplement(DNAObject) 
	revString <- toString(revStrand)
	noquote(revString) 
}