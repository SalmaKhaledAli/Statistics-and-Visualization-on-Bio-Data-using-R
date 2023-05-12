#DNA_seq <- readline(prompt = "Enter The DNA Sequence : ")

DNA_to_RNA <- function(seq){
  
  seq_splitted <- strsplit(seq,'') [[1]]
  
  for (i in 1:nchar(seq))
  {
    if(seq_splitted[i]=='T'){
      
      seq_splitted[i] <- 'U'
      
    }
  }
  seq_splitted
  print(seq_splitted)
}

#DNA_to_RNA(DNA_seq)

