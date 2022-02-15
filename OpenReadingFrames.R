# ROSALIND "OPEN READING FRAMES" solved by Anh Vo 

# This program reads a given FASTA file containing a DNA string
# and translates an open reading frame (ORF) into amino acids.
# Outputs an array of unique protein strings found. 

# Requires: R version 4.1, Biostrings, and stringr package

BiocManager::install("Biostrings")
install.packages("stringr")

library(Biostrings)
library(stringr)


# Create a reference DNA codon table 

createDNA_tab <- function() {
  amino <- c("TTT","TTC","TTA","TTG","TCT","TCC","TCA","TCG",
             "TAT","TAC","TAA","TAG","TGT","TGC","TGA","TGG",
             "CTT","CTC","CTA","CTG","CCT","CCC","CCA","CCG",
             "CAT","CAC","CAA","CAG","CGT","CGC","CGA","CGG",
             "ATT","ATC","ATA","ATG","ACT","ACC","ACA","ACG",
             "AAT","AAC","AAA","AAG","AGT","AGC","AGA","AGG",
             "GTT","GTC","GTA","GTG","GCT","GCC","GCA","GCG",
             "GAT","GAC","GAA","GAG","GGT","GGC","GGA","GGG")
  
  
  protein <- c("F","F","L","L","S","S","S","S","Y","Y","Stop",
               "Stop","C","C","Stop","W","L","L","L","L","P",
               "P","P","P","H","H","Q","Q","R","R","R","R","I",
               "I","I","M","T","T","T","T","N","N","K","K","S",
               "S","R","R","V","V","V","V","A","A","A","A","D",
               "D","E","E","G","G","G","G")
  
  codonTab <- c(amino, protein)
  colnames(codonTab) <- NULL
  codonTab <- cbind(amino, protein)
}

  
# Iterate through given DNA string and translates ORF into amino acids
# Returns an array of protein strings found
ORF_protein <- function(sub) {
  
  currStr <- NULL
  newStr <- NULL
  start = ""
  index = 1
  list <- NULL
  newList <- NULL
  
  # Iterate through DNA string and find ORFs
  for (i in 1:(trunc(nchar(sub)/3))) {
    subS<- substr(sub,index, index+2) # selects current codon

    if (subS == "ATG") { # finds start codon and set start variable
      start = "yes"
    } 
    
    # If ORF found, saves protein string found to an array
    # Resets start variable
    if (start == "yes" & codonTab[grep(subS,codonTab),2] == "Stop") {
      newList <- rbind(list, currStr)
      list <- newList
      currStr = ""
      start = "stop"
    }
    
    
    # If stop codon not found, continue to assemble protein strand
    if (start== "yes" & codonTab[grep(subS,codonTab),2] != "Stop") {
        newStr <- paste(currStr, codonTab[grep(subS,codonTab),2], sep="")
        currStr <- newStr
    }
    
    index = index + 3 # reset variable to next codon
  }
  
  return (list)
}

# Returns complement DNA strand
revDNA <- function(s) {
  DNAObject <- DNAString(s)
  revStrand <- reverseComplement(DNAObject) 
  revString <- toString(revStrand)
  return (noquote(revString)) 
}

# Find proteins in ORF and returns an array of unique protein strands
find_ORF <- function(s) {
  
  currList <- NULL
  newList <- NULL
  start = 1
  
  # Start at each position in DNA strand to find all possible ORFs
  # Passes new starting position to translate amino acids
  for (i in 1:(nchar(s)-3)) {
    sub <- substr(s, start, nchar(s))
    newList <- rbind(currList,unique(ORF_protein(sub)))
    currList <- newList
    start = start + 1
  }
  return (unique(currList))
  
}

# Read a given FASTA file with a DNA string
# Extracts and combine each line and returns a final DNA string
readFASTA <- function(FASTA) {
  line <- readLines(FASTA)
  
  s <- NULL
  newString <- NULL
  for (i in line) {
    if (grepl(">", i) == FALSE) {
      newString <- paste(s, i, sep="")
      s <- newString
    }
  }
  return (s)
}

# Main function to read DNA string from FASTA file
# Create a DNA codon table
# Translate protein string from forward and reverse strands
# from ORFs at any given position on DNA strand.
# Outputs a final array of unique protein strand found
main <- function(FASTA) {
  createDNA_tab()
  s <- readFASTA(FASTA)
  list1 <- find_ORF(s)
  rev <- revDNA(s)
  list2 <- find_ORF(rev)
  uniqList <- (unique(rbind(list1,list2)))
  rownames(uniqList) <- NULL
  print(noquote(uniqList))
}