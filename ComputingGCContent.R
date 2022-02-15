# ROSALIND "COMPUTING GC CONTENT" solved by Anh Vo

# Reads a given FASTA file to compute highest GC content strand. 
# Outputs strand name and percent GC. 


# Requires "stringr" package

install.packages("stringr")
library("stringr")

GC_content <- function(FASTA) {
  line <- readLines(FASTA)
 
  # Set variables for GC counts
  
  currGC = 0;
  newGC = 0;
  currStrandLength = 0;
  newStrandLength = 0;
  maxGC = 0;
  GC_count = 0;
  strandName = ""
  maxStrandName = ""
  
  # Read through each FASTA file line, count GC and get strand length 
  # Stores current max GC count and strand name
  
  for(i in line) {
    
    if (grepl(">", i) == FALSE) {
      newGC = sum(currGC, str_count(i, "G"), str_count(i,"C"))
      newStrandLength = sum(currStrandLength, nchar(i))
      
    } else {
      GC_count = (currGC/currStrandLength)
      if (GC_count == "NaN") {
        GC_count = 0
      }
      
      if (GC_count > maxGC) {
        maxGC = GC_count 
        maxStrandName = strandName
      }
      newGC = 0
      newStrandLength = 0
      strandName = i
    }
    currGC = newGC
    currStrandLength = newStrandLength
  } 
  
  GC_count = (currGC/currStrandLength)
  if (GC_count > maxGC) {
    maxGC = GC_count 
    maxStrandName = strandName
  }
  print(noquote(substr(maxStrandName,2,nchar(maxStrandName))))
  print(maxGC*100)

}
     



