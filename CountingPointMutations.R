# ROSALIND "COUNTING POINT MUTATIONS" solved by Anh Vo


# Compare two given DNA strings and outputs Hamming distance

# Requires stringr package

install.packages("stringr")
library(stringr)

ham_distance <- function(s, t) {
  string1 <- strsplit(s, "")[[1]]
  string2 <- strsplit(t, "")[[1]]
  
  index = 0; 
  count = 0;
  for (i in string1) {
    index = index + 1;
    if (i != string2[index]) {
      count = count + 1
    }
  }
  
  print(count)
}