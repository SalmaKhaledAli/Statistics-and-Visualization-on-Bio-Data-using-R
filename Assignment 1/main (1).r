# Define the DNA_to_RNA.R script

source("C:/Users/Salma/Downloads/DNA_to_RNA.R")

DNA_seq <- readline(prompt = "Enter The DNA Sequence : ")

paste(DNA_to_RNA(DNA_seq), collapse = "")
