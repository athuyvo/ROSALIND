# ROSALIND "TRANSCRIBING DNA INTO RNA" solved by Anh Vo 

# Outputs protein corresponding to a given RNA string

# Requires string package

install.packages("stringr")
library(stringr)

# Create a reference RNA codon table 

createRNA_tab <- function() {
  amino <- c("UUU","UUC","UUA","UUG","UCU","UCC","UCA","UCG",
             "UAU","UAC","UAA","UAG","UGU","UGC","UGA","UGG",
             "CUU","CUC","CUA","CUG","CCU","CCC","CCA","CCG",
             "CAU","CAC","CAA","CAG","CGU","CGC","CGA","CGG",
             "AUU","AUC","AUA","AUG","ACU","ACC","ACA","ACG",
             "AAU","AAC","AAA","AAG","AGU","AGC","AGA","AGG",
             "GUU","GUC","GUA","GUG","GCU","GCC","GCA","GCG",
             "GAU","GAC","GAA","GAG","GGU","GGC","GGA","GGG",
             "AUG")
  protein <- c("F","F","L","L","S","S","S","S","Y","Y","Stop",
               "Stop","C","C","Stop","W","L","L","L","L","P",
               "P","P","P","H","H","Q","Q","R","R","R","R","I",
               "I","I","M","T","T","T","T","N","N","K","K","S",
               "S","R","R","V","V","V","V","A","A","A","A","D",
               "D","E","E","G","G","G","G", "Start")
  
  codonTab <- c(amino, protein)
  colnames(codonTab) <- NULL
  codonTab <- cbind(amino, protein)
}

# Iterate through given RNA string and outputs corresponding protein
getProtein <- function(s) {
  currStr <- NULL
  newStr <- NULL
  
  index = 1
  
  for (i in 1:(nchar(s)/3)) {
    
    subS<- substr(s,index, index+2)
    if (codonTab[grep(subS,codonTab),2] != "Stop") {
      
      if (codonTab[grep(subS,codonTab),2] != "Start")
        newStr <- paste(currStr, codonTab[grep(subS,codonTab),2], sep="")
        currStr <- newStr
    }
    
    index = index + 3
    
  } else {
    break
  }
}

  print(noquote(currStr))
}


main <- function() {
  createRNA_tab()
  getProtein(s)
}
  


