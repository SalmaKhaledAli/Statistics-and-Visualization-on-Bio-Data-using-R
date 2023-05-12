# Read the file 
path.name <- "C:/Users/Salma/Downloads/Assignment_04_P_values.txt"
dataFrame   <- read.table(path.name, sep = '\t')
dataFrame

p_values = c()


for (i in 1:nrow(dataFrame)){
 p_values[i] <- as.double(dataFrame[i,])
}
  
p_values
no_p_values <- length(p_values)

# Apply the two correction methods Bonferroni and FDR to reduce false positives
q_bonf = p.adjust(p_values, method= 'bonferroni')
q_fdr = p.adjust(p_values, method= "fdr")


# function returns the number of significant p values 
Counting <- function(plist){
  q_bonf_sig = c()
  for(j in plist){
    if(j <= 0.05){
      q_bonf_sig <- append(q_bonf_sig, j)
    
    }
  }
  no_q_values_sig <- length(q_bonf_sig)
  no_q_values_sig
}

#The number of the p_values before any correction.
print("The number of the p_values before any correction.")
print(no_p_values)
#The number of significant p_values before any correction.
print("The number of significant p_values before any correction.")
print(Counting(p_values))
#The number of significant p_values after applying the Bonferroni method.
print("The number of significant p_values after applying the Bonferroni method.")
print(Counting(q_bonf))
#The number of the significant p_values after applying the FDR method.
print("The number of the significant p_values after applying the FDR method.")
print(Counting(q_fdr))

# Save the results in a txt file
Data <- capture.output(print("The number of the p_values before any correction."),
                       print(no_p_values),
                       print("The number of significant p_values before any correction."),
                       print(Counting(p_values)),
                       print("The number of significant p_values after applying the Bonferroni method."),
                       print(Counting(q_bonf)),
                       print("The number of the significant p_values after applying the FDR method."),
                       print(Counting(q_fdr))
                       )
                       

writeLines(Data, con = file("C:/Users/Salma/Downloads/Assignement_04_file.txt"))