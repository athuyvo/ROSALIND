# ROSALIND "FINDING A MOTIF IN DNA" solved by Anh Vo 

# Find location of a given motif in a given DNA or RNA string.
# Output starting locations of each motif found.

# Requires stringr package. 

install.packages("stringr")
library("stringr")

find_motif <- function(s, t) {
  
  
  currStr <- "";
  newStr <- "";
  
  if (grepl(t, s) == FALSE) {
    print("0")
    
  } else {
    
    strand <- strsplit(s, "")[[1]]
    motif <- strsplit(t, "")[[1]]
    index = 0;
    
    for (i in strand) {
      index = index + 1
      
      # If strand has first motif bp,
      # create substring in strand and find motif 
      if (i == motif[1]) {
        substrand = substring(s,index,(index+nchar(t)-1))
        
        if (grepl(t, substrand) == TRUE) {
          newStr <- paste(currStr, index, sep=" ")
          currStr <- newStr
        }
        
      }
    }
    
    print(noquote(newStr))
  }

}

