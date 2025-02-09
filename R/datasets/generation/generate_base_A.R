library(tictoc)
library(magrittr)
library(dplyr)
library(mlr)
library(corrplot)
library(caret)
library(devtools)
library(ggbiplot)

#### Loading dataset from file. Loading data generated by source forge scrapper:
data <- read.csv('R/datasets/dataset.csv', header = TRUE, stringsAsFactors = FALSE)


tic("execution time: ")
newData <- data %>% na_if("") %>% na.omit(data)
toc()


newData <- rename(newData, replace = c("OperationSystems" = "OperatingSystems"))
write.csv(newData,'R/datasets/data_set_base_A.csv', row.names = FALSE)
