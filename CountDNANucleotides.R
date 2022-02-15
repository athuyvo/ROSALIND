# ROSALIND "COUNTING DNA NUCLEOTIDES" solved by Anh Vo


# Outputs number of each base from a given DNA string in format 'A', 'C', 'G', 'T'
num_bases <- function(s) {
	if (nchar(s) == 0) {
		return (c(0,0,0,0))
	} 
	
	A = 0
	C = 0
	G = 0
	T = 0

	split <- strsplit(s, "")[[1]]

	for (i in split) {
		if (i == "A") { 
			A = A + 1 
		} else if (i == "C") {
			C = C + 1
		} else if (i == "G") {
			G = G + 1
		} else if (i == "T") {
			T = T + 1
		}
	}

	return (c(A, C, G, T))
}