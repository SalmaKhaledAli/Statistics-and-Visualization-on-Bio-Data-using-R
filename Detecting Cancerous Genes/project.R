library(dplyr)

cancer_data <- read.delim("C:/Users/Salma/Downloads/Project_Data/Project_Data/kirc-rsem-fpkm-tcga_paired.txt", header = TRUE, sep = "\t", dec = ".")
healthy_data <- read.delim("C:/Users/Salma/Downloads/Project_Data/Project_Data/kirc-rsem-fpkm-tcga-t_paired.txt", header = TRUE, sep = "\t", dec = ".")

Remove_Na <- function(Data_frame){
  
  Data_frame %>% 
    dplyr::mutate_if(is.numeric, function(x) ifelse(is.na(x), median(x, na.rm = T), x))
}

preprocessing <- function(cdata, hdata){
  
  filtered_kirc_cancer <- cancer_data[rowSums(cancer_data == 0) <= 25, ]
  filtered_kirc_cancer
  
  filtered_kirc_Healthy <- healthy_data[rowSums(healthy_data == 0) <= 25, ]
  filtered_kirc_Healthy
  
  pre_Healthy_data <- filtered_kirc_Healthy[filtered_kirc_Healthy$Hugo_Symbol %in% filtered_kirc_cancer$Hugo_Symbol, ]
  pre_Healthy_data
  
  pre_Cancer_data <- filtered_kirc_cancer[filtered_kirc_cancer$Hugo_Symbol %in% filtered_kirc_Healthy$Hugo_Symbol,]
  pre_Cancer_data
  
  pre_Healthy_data <- Remove_Na(pre_Healthy_data)
  pre_Cancer_data <- Remove_Na(pre_Cancer_data)
  
  
}
 

#loop to test each row per dataset of kirc
for (i in 1:nrow(pre_Cancer_data)){
  cancer_vector <- as.double(pre_Cancer_data[c(i),])
  healthy_vector <- as.double(pre_Healthy_data[c(i),])
  #implement wilcoxcn test
  p_values= c()
  test = wilcox.test(cancer_vector[3:ncol(pre_Cancer_data)],healthy_vector[3:ncol(pre_Healthy_data)], alternative = 'two.sided', paired = TRUE)
  #p_values" <- append(test$p.value, i)
  #new_p = test$p.value
  #p_values <- c(p_values, new_p)
 
  file_path <- "C:/Users/Salma/Downloads/p_values.txt"
  write(test$p.value, file = file_path, append = TRUE)
  
}

#Implement FDR

file.name <- read.table(file = file_path, sep = "", header=FALSE)

p_values2 = c()

for (i in 1:nrow(file.name)){
  p_values2[i] <- as.double(file.name[i,])
}


p_adjust_value = p.adjust(p_values2, method= "fdr")
p_sign = c()
is_sign = c()
for (i in p_adjust_value){
  if(i < 0.05){
    p_sign <- append(p_sign, i)
    is_sign <- append(is_sign, T)
  }
  else{
    is_sign <- append(is_sign, F)
  }
}
print(p_sign)


df <- data.frame(Gene_name = pre_Cancer_data[[1]], P_values = p_values2, Adjusted_value = p_adjust_value, is_significant = is_sign)
df
write.table( df[order(df$Adjusted_value),], "C:/Users/Salma/Downloads/Genes.txt")


