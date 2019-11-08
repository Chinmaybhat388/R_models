getwd()
setwd('D:/DATA science/datasets/Machine-Learning')

raw_data <- read.csv('wisc_bc_data.csv',stringsAsFactors = FALSE)

View(raw_data)
str(raw_data)
table(raw_data$diagnosis)

#Shuffle the rows of the data

set.seed(42)
rows <- sample(nrow(raw_data))
raw_data <- raw_data[rows,]
raw_data[1,]

rownames(raw_data) <- seq(length = nrow(raw_data))


raw_data$diagnosis <- factor(raw_data$diagnosis,levels = c('B','M'),labels = c('Benign','Malignant'))


round(prop.table(table(raw_data$diagnosis)) * 100)
#63% people were diagnosed as benign and 37% as malignant.

data <- raw_data[ ,-1]
View(data)

normalize <- function(x) {
  return ((x - min(x))/(max(x) - min(x)))
}


data_normalized <- as.data.frame(lapply(data[ ,2:31], normalize))
View(data_normalized)

data_normalized['diagnosis'] <- data[ ,'diagnosis']

library(caret)

train_index <- createDataPartition(raw_data$diagnosis,p = 0.7,list = FALSE,times = 1)

cancer_train <- data_normalized[train_index, ]
nrow(cancer_train)
View(cancer_train)
cancer_train_labels <- cancer_train[ ,'diagnosis']
View(cancer_train_labels)
#cancer_train_labels <- as.data.frame(cancer_train_labels)
#colnames(cancer_train_labels) <- c("diagnosis")

class(cancer_train_labels)

#remove the classifying label from the train data
cancer_train <- cancer_train[ ,-31]

#Create test data and remove the classifying label from the data.
cancer_test <- data_normalized[-train_index, ]
cancer_test_labels <- cancer_test[ ,'diagnosis']
View(cancer_test_labels)


cancer_test <- cancer_test[ ,-31]


#KNN
library(class)

cancer_predicton <- knn(train = cancer_train,test = cancer_test,cl = cancer_train_labels,k = 21)
summary(cancer_predicton)

#Verifying the classification

library(gmodels)

CrossTable(cancer_test_labels,cancer_predicton,prop.chisq = FALSE)
#KNN with k=21 has given an accuracy of 95.3%, TPR of 89% and TNR of 99%.

# The Recall would be 89% and Precision would be 98%.
#In this scenario, We need to reduce the False Negative.
#Recall is more critical in this scenario.


#Trying k=12

cancer_prediction_12 <- knn(cancer_train,cancer_test,cancer_train_labels,12)

summary(cancer_prediction_12)

CrossTable(cancer_test_labels,cancer_prediction_12,prop.chisq = FALSE)

#Slightly beter result with k=12.
#The Recall would be 90.4% and Precision would be 98%.
