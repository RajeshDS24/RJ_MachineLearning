####################################################### 
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
##              ## Author : Rajesh RJ                ##  
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#######################################################



## Installing Required R packages
install.packages("ggplot2") # For data visualization 
install.packages("dplyr") # For Data manipulation
install.packages("caret") # for data modeling
install.packages("NeuralNetTools") # For Neural Networks
install.packages("funModeling")


# Adding libraries
library(dplyr)
library(ggplot2)
library(caret)
library(NeuralNetTools)
library(funModeling)

# Set the path to load the data to R
setwd("E:/ICT619 - AI/Assignment1/DataSet")

WBC_Data <- read.csv("data.csv") %>% na.omit()

# To see the whole data set
View(WBC_Data)

## From CRISP-DM framework
### Data understanding
colnames(WBC_Data)

# Find number of classes in the output variables.
class(WBC_Data$diagnosis)

# to check the Rows and Columns:
dim(WBC_Data) 

# To check the data type and data format
str(WBC_Data)

## check null values
is.null(WBC_Data)  ## returns boolean

which(is.na(WBC_Data))  ## Returns int

# 
table(WBC_Data$diagnosis)



## CRISP-DM 
## ~~~~~~~~~~~~~~~~~~~  Data Modeling

## the data set has unwanted columns so removing them 
WBC_Data <- WBC_Data[c(-1,-33)]

## Check the columns again 
colnames(WBC_Data)

## set seed value so that when ever we run the model we can take same Train and test data:
set.seed(143)

# Creating the partition for training and testing
Partition_data <- createDataPartition(WBC_Data$diagnosis, times = 1, p = 0.8, list = F)
head(Partition_data)

# Train data
Train_Data <- WBC_Data[Partition_data,]


# Test data
Test_Data <- WBC_Data[-Partition_data, ]


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# funModeling package

plot_num(mean_data)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~ Random Forest

RF.model <- train(diagnosis ~., Train_Data, method = "rf")

RF.Predict <- predict(RF.model, Test_Data)
Confusion_matrix <- confusionMatrix(RF.Predict, Test_Data$diagnosis, positive = "M")
Confusion_matrix




# ~~~  Decision Tree

DT.model <- train(diagnosis ~., Train_Data, method = "rpart")

DT.Predict <- predict(DT.model, Test_Data)
Confusion_matrix2 <- confusionMatrix(DT.Predict, Test_Data$diagnosis, positive = "M")
Confusion_matrix2



##  ~~ Neural Net
install.packages("nnet")
library(nnet)

Neural_Net <- train(diagnosis ~.,Train_Data, method = "nnet")
Neural_Net

Neural_net.pred <- predict(Neural_Net, Test_Data)

Model_accuracy1 <- confusionMatrix(Neural_net.pred, Test_Data$diagnosis, positive = "M")
Model_accuracy1

## Nueral network structure
plotnet(Neural_Net)


## Correlation funtion

Corr_matrix <-cor(MASS_data1)
corrplot(Corr_matrix, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))



## PCA can be used as we have continues data.






















