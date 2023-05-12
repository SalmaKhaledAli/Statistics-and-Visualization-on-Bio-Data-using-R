###### Part 1 ##########

# 1 - read the csv file and store the data in the dataframe

path.name <- "C:/Users/Salma/Downloads/Assignment_02_Data.csv"
GE.data   <- read.table(path.name, sep = ',', header=T, row.names = 1)
GE.data

# 2 - Get the Median of the column and impute the data in NA values
library(dplyr)
Remove_Na <- function(Data_frame){
  
  Data_frame %>% 
  dplyr::mutate_if(is.numeric, function(x) ifelse(is.na(x), median(x, na.rm = T), x))
}

new_data <- Remove_Na(GE.data)

# 6 - “GE_Data_Modification.R” receives the output data frame from “GE_Data_Normalization.R” and then 
#      stores it in a tab “\” delimited text file

source("C:/Users/Salma/Downloads/GE_Data_Normalization.R")
text_file_path <- "C:/Users/Salma/Downloads/GE_Modified_data.txt"

Modified_data <- GE_Data_Normalization(new_data)
write.table(Modified_data, file = text_file_path, sep = "\t", col.names = TRUE, row.names = TRUE)

######### Part 2 ################

GE.mean   <- apply(new_data, 2, mean)
GE.sd <- apply(new_data, 2, sd)



path.name <- "C:/Users/Salma/Downloads/Assignment2_13.png"
png(path.name, width = 2000, height = 1600, res=200)

layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = FALSE))
X <- c(1:5)
y_lim = c(0, max(GE.sd))
# First Figure
plot(X, GE.mean, xlim = c(0,7), ylim = y_lim, type = "b", lty = 1, pch = 10, 
     col = "red", xlab = "Genes_samples", ylab = "Mean & Standard Deviation",
     main = 'Mean and Standard Deviation values')
lines(X, GE.sd, pch = 10, col = "blue", type = "b", lty = 2,)
legend("topleft",legend=c("Mean", "SD"), col=c("red", "blue"), lty = 1:2)

# Second Figure
boxplot(GE.mean, GE.sd, ylim = y_lim, names = c('Mean', 'Standard Deviation'),
        xlab = 'Genes', ylab = 'Value',
        main = 'Distribution of the Mean and SD values')
# Third Figure
plot(ecdf(GE.mean), xlab = 'Genes Samples', main = 'CDF Of The Means', ylab = 'Means',)


dev.off()



