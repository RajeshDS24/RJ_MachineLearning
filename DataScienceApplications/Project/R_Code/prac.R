########################################## 
##########################################        

#      unit    : ICT583 - DSA
#      Author  : Rajesh Jyothi
#      StudID  : 33669079
#      Task    : Project

##########################################   
########################################## 

# For data manipulation
install.packages("dplyr") 
library(dplyr)

# For data visualization 
install.packages("ggplot2")
library(ggplot2)

## Load VIM package for Visualization and Imputation of Missing Values
install.packages("VIM")
library(VIM)

# For better colors in the plot
install.packages("RColorBrewer")
library(RColorBrewer)

# For different color shades
install.packages("wesanderson")
library(wesanderson)

# To Plot the missing values
install.packages("naniar")
library(naniar)

install.packages("caret")
library(caret)

install.packages("nnet")
library(nnet)

install.packages("NeuralNetTools")
library(NeuralNetTools)

# To access the Decision Tree model
install.packages("rpart")
library(rpart)

# To plot the Decision Tree model
install.packages("rpart.plot")	
library(rpart.plot)

# Random Forest model
install.packages("randomForest")	
library(party)  ### ----------- for cforest
library(randomForest)

install.packages("class")
library(class)

install.packages("pROC")
library(pROC)

# Get the path 
getwd()

# Reading the dataset
MASS_data <- read.csv("CancerData.csv")


# View the data
View(MASS_data)

# Check the data size
dim(MASS_data) # 961   6

# check the structure of the dataset 
str(MASS_data) ## All the data types are numeric

# View the sample data
head(MASS_data)

# Summarize the attribute 
summary(MASS_data)

# Checking the missing values
sum(is.na(MASS_data))

# columnWise missing values
missing_data <- colSums(is.na(MASS_data))
missing_data

# Ploting missing data in the data set
gg_miss_var(MASS_data)


# Percentage of missing data
gg_miss_var(MASS_data, show_pct = TRUE)

# --- or


## Visualizing the missing values using VIM package
aggr(MASS_data, prop = F, numbers = T, col = "orange")


# visualizing null values in r
missing_data <- miss_var_summary(MASS_data) 
ggplot(missing_data, aes(x = missing_data$variable, y = missing_data$n_miss, fill = missing_data$variable)) + 
  geom_bar(stat="identity") + scale_fill_brewer(palette="RdPu") + 
  labs(title = "Displaying Missing Data Values in Bar chart", x = "Factors effecting the Breast Cancer", 
       y = "Number of missing values")


# As the missing values are randomly distributed. we shall drop the missing values.
# Omitting the missing values in the dataset.
MASS_CleanData <- MASS_data %>% na.omit()
View(MASS_CleanData)

dim(MASS_CleanData)




## Data Tranformation
MASS_CleanData$BI.RADS <- as.factor(MASS_CleanData$BI.RADS)
MASS_CleanData$Shape <- as.factor(MASS_CleanData$Shape) 
MASS_CleanData$Margin <- as.factor(MASS_CleanData$Margin) 
MASS_CleanData$Density <- as.factor(MASS_CleanData$Density) 
MASS_CleanData$Severity <- as.factor(MASS_CleanData$Severity) 


attach(MASS_CleanData)

MASS_CleanData %>% ggplot(aes(x = Severity, y = Age)) + geom_boxplot()

# Visualizing the data
### 
ggplot(MASS_CleanData, aes(x = Age, fill = BI.RADS)) + geom_bar( color = "black") + 
  theme_minimal() + labs(title="Histogram plot to show the severity based on age", x= "Density", y="Age" )


ggplot(MASS_CleanData, aes(x = Age, fill = Severity)) + geom_histogram( color = "black") + 
  theme_minimal() + labs(title="Histogram plot to show the severity based on age", x= "Density", y="Age" )


ggplot(MASS_CleanData) + geom_bar(aes(Shape)) + 
  theme_minimal() + labs(title="Barplot to show the severity based on age ",x=" Shape")

ggplot(MASS_CleanData) + geom_bar(aes(Margin)) + 
  theme_minimal() + labs(title="Barplot to show the Margin class data ",x=" Margin")

ggplot(MASS_CleanData) + geom_bar(aes(Density)) + 
  theme_minimal() + labs(title="Barplot to show the Density class data ",x=" Density")



ggplot(MASS_CleanData, aes(y = Age, x= Shape,  fill = Severity)) + 
  geom_bar( stat = "identity", position = "dodge") +
  theme_classic() + theme(legend.position="top")

ggplot(MASS_CleanData, aes(y = Age, x= Margin,  fill = Severity)) + 
  geom_bar( stat = "identity", position = "dodge") +
  theme_classic() + theme(legend.position="top")

ggplot(MASS_CleanData, aes(y = Age, x= Density,  fill = Severity)) + 
  geom_bar( stat = "identity", position = "dodge") +
  theme_classic() + theme(legend.position="top")


ggplot(MASS_CleanData, aes(x = Age, fill = Severity)) + 
  geom_histogram(color = "black") + scale_color_brewer(palette="Accent") + 
  theme_classic() + theme(legend.position="top")


#### answering the Questions 
# 1) What shape of the cancer is most common in Malignant cancer
Mal_Cancer <- MASS_CleanData %>% subset(Severity == 1)
plot_1 <- data.frame(table(Mal_Cancer$Shape))
plot_1
ggplot(plot_1, aes(x  = Var1, y = Freq, fill = Var1)) + geom_bar(stat = "identity", color = "black") + 
  labs(title="Barplot to Find the common shape in Malignant Cancer ",x=" Shape of the tissue")

# type 4 is more dominant in malignant patients.

# 2) What is the common density of breast mass incase of malignant patients.

plot_2 <- data.frame(table(Mal_Cancer$Density))
plot_2
ggplot(plot_2, aes(x  = Var1, y = Freq, fill = Var1)) + geom_bar(stat = "identity", color = "black") + 
  labs(title="Barplot to Find the common level of Density in Malignant Cancer ",x=" Density of the tissue")

# low Density.


## Data partition
Data_Partition <- createDataPartition(MASS_CleanData$Severity, times = 1, p = 0.7, list = F)



# Train data
Train_Data <- MASS_CleanData[Data_Partition,]


# Test data
Test_Data <- MASS_CleanData[-Data_Partition, ]



# ~~~  Decision Tree

Decision_Tree <- train(Severity ~., Train_Data, method = "rpart")

Predict_DT <- predict(Decision_Tree, Test_Data)
Confusion_matrix1 <- confusionMatrix(Predict_DT, Test_Data$Severity, positive = "1")
Confusion_matrix1


fit <- rpart(Severity ~., data = Train_Data, method = 'class')
rpart.plot(fit, extra = 106)


## tunning the hyper parameters
rpart.control(minsplit = 20, minbucket = round(minsplit/3), maxdepth = 30)

accuracy_tune <- function(fit) {
  predict_unseen <- predict(fit, Test_Data, type = 'class')
  table_mat <- table(Test_Data$Severity, predict_unseen)
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  accuracy_Test
}


control <- rpart.control(minsplit = 4,
                         minbucket = round(5 / 3),
                         maxdepth = 3,
                         cp = 0)

tune_fit <- rpart(Severity ~., data = Train_Data, method = 'class', control = control)
accuracy_tune(tune_fit)
## This gives the accuracy of 0.8346774


# ~~ Random Forest 

Random_Forest <- train(Severity ~., Train_Data, method = "rf")

Predict_RF <- predict(Random_Forest, Test_Data)
Confusion_matrix2 <- confusionMatrix(Predict_RF, Test_Data$Severity, positive = "1")
Confusion_matrix2


cforest(Severity ~ ., data = MASS_CleanData, controls=cforest_control(mtry=2, mincriterion=0))

plot(randomForest(Severity ~ ., MASS_CleanData, keep.forest=FALSE, ntree=100), log="y")




# ~~~ logistic Regression

Logistic_Reg <- train(Severity ~., Train_Data, method = "glm")

Predict_LR <- predict(Logistic_Reg, Test_Data)
Confusion_matrix3 <- confusionMatrix(Predict_LR, Test_Data$Severity, positive = "1")
Confusion_matrix3

# ~~~~ Nueral Networks

Neural_Net <- train(Severity ~.,Train_Data, method = "nnet")

Predict_NN <- predict(Neural_Net, Test_Data)
Confusion_matrix4 <- confusionMatrix(Predict_NN, Test_Data$Severity, positive = "1")
Confusion_matrix4

## Nueral network structure
plotnet(Neural_Net)

# ~~~~ KNN

KNN_Model <- train(Severity ~.,Train_Data, method = "knn")

Predict_KNN <- predict(KNN_Model, Test_Data)
Confusion_matrix5 <- confusionMatrix(Predict_KNN, Test_Data$Severity, positive = "1")
Confusion_matrix5


# ~~~~~~ KNN from class package
KNN_Model_1 <- knn(train = Train_Data, test = Test_Data,cl = Train_Data$Severity, k=10)

matrix <- table(KNN_Model_1, Test_Data$Severity)
##check the accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(matrix)


# -- Tuning the KNN
i=1                     
k.optm=1                     
for (i in 1:28){ 
  knn.mod2 <-  knn(train=Train_Data, test=Test_Data, Train_Data$Severity, k=i)
  k.optm[i] <- 100 * sum(Test_Data$Severity == knn.mod2)/NROW(Test_Data$Severity)
  k=i  
  cat(k,'=',k.optm[i],'\n')       # to print % accuracy 
}

plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level") 


# Selecting the optimal K value from the above plot
KNN_Model_2 <- knn(train = Train_Data, test = Test_Data,cl = Train_Data$Severity, k=1)
matrix_2 <- table(KNN_Model_2, Test_Data$Severity)
##check the accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(matrix_2)






