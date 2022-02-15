# ROSALIND "OPEN READING FRAMES" solved by Anh Vo 

install.packages("stringr")
library("stringr")

# Transcribes a given DNA string and outputs RNA string 
transcribe <- function(s) {
	if (nchar(s) ==0) {
		return (NULL)
	}

	print(str_replace_all(s,"T", "U"))
}