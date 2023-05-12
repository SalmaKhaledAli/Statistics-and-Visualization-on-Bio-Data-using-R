source("C:/Users/Salma/Downloads/Rproject/Project_Part1.R")

## Kindey genes ##
kidney_control <- read.delim("C:/Users/Salma/Downloads/Project_Data/Project_Data/kirc-rsem-fpkm-tcga_paired.txt", header = TRUE, sep = "\t", dec = ".")
kidney_condition <- read.delim("C:/Users/Salma/Downloads/Project_Data/Project_Data/kirc-rsem-fpkm-tcga-t_paired.txt", header = TRUE, sep = "\t", dec = ".")
## Lung genes ##
lung_control <- read.delim("C:/Users/Salma/Downloads/Rproject/Project_Data/lusc-rsem-fpkm-tcga_paired.txt", header = TRUE, sep = "\t", dec = ".")
lung_condition <- read.delim("C:/Users/Salma/Downloads/Rproject/Project_Data/lusc-rsem-fpkm-tcga-t_paired.txt", header = TRUE, sep = "\t", dec = ".")
#### Preprocessing On Kidney Genes ####
Full_kidney_data <-  preprocessing(kidney_condition, kidney_control)
pre_kidney_Healthy_data <- Full_kidney_data$Healthy_data
pre_kidney_Cancer_data <- Full_kidney_data$cancer_data
#### Apply the Hypothesis Testing on Kindey Genes ####

Hypothesis_Testing(pre_kidney_Cancer_data, pre_kidney_Healthy_data, TRUE)
Hypothesis_Testing(pre_kidney_Cancer_data, pre_kidney_Healthy_data, FALSE)


#### Preprocessing On Lung Genes ####
Full_lung_data <-  preprocessing(lung_condition, lung_control)
pre_lung_Healthy_data <- Full_lung_data$Healthy_data
pre_lung_Cancer_data <- Full_lung_data$cancer_data
#### Apply the Hypothesis Testing on lung Genes ####
Hypothesis_Testing(pre_lung_Cancer_data, pre_lung_Healthy_data, TRUE)
Hypothesis_Testing(pre_lung_Cancer_data, pre_lung_Healthy_data, FALSE)
