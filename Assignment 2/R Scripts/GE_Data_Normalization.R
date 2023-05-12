
# 3 - Send the imputed data to the “GE_Data_Normalization.R”

source("C:/Users/Salma/Downloads/GE_Data_Modification.R")

data <- Remove_Na(GE.data)
data3 <- data

# 4 - “GE_Data_Normalization.R” replaces each value in the data frame by a new value computed
#      as:   xnew=  xold−xavg / xsd

GE_Data_Normalization <- function (dataframe){
  for (i in 1:ncol(dataframe)){
    x_sd <- sd(dataframe[, i])
    x_avg <- mean(dataframe[, i])
    for (j in 1:nrow(dataframe)){
      
      #print(data[j, i])
      dataframe[j, i] <- (dataframe[j, i] - x_avg) / x_sd
    }
  }
  print(dataframe)
}

# 5 - The output of “GE_Data_Normalization.R” is a data frame containing the normalized GE values.

GE_Data_Normalization(data3)
