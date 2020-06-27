################################################### 
###################################################


##        Unit    : ICT619 AI 
##        Authors : Amanjot  (33516689) 
##                  Pardeep  (33668635)       
##                  Shilpa   (33305704) 
##                  Jaspreet (33340641) 

###################################################
###################################################


## Installing Required R packages
install.packages("ggplot2")
library(ggplot2)

install.packages("dplyr")
library(dplyr)

install.packages("caret")
library(caret)

install.packages("nnet")
library(nnet)

install.packages("NeuralNetTools")
library(NeuralNetTools)

install.packages("RColorBrewer")
library(RColorBrewer)

install.packages("corrplot")
library(corrplot)

install.packages("GGally") # For ggcorr plot.
library(GGally)

install.packages("farver") 
library(farver)


# get data
Raw_cancer_data <- read.csv("data.csv")

View(Raw_cancer_data)

###########################
#   Data  understanding   #
###########################

### Data understanding
## For column names:
colnames(Raw_cancer_data)

## Rows and Columns:
dim(Raw_cancer_data)

##  Check column types
str(Raw_cancer_data)

## summary of the data set
summary(Raw_cancer_data)

# Check the number of classes in the cancer data set.
levels(cancer_data$diagnosis)

# Data distribution
table(Raw_cancer_data$diagnosis)

## check null values
is.null(Raw_cancer_data) ## False
# So no missing data.

unique(Raw_cancer_data)
# check for duplicate records/observations

count(distinct(Raw_cancer_data))

# Removing the unwanted columns
cancer_data <- Raw_cancer_data[c(-1,-33)]

## visualizing cancer data.
# View the proportion of the classes in Diagnosis
ggplot(cancer_data, aes(x = factor(diagnosis), fill = diagnosis )) + 
  labs(title = "Distribution of Observations in the dataset.", x = "Diagnosis", y = "Number of observations" ) + 
  geom_bar(color = "black") + scale_fill_brewer(palette = "Paired")

## plot the correlation plot for Mean, SE and Worst.

mean_data <- round(cancer_data[,c(2:11)], 2)
colnames(mean_data)

SD_data <- round(cancer_data[,c(12:21)], 2)
colnames(SD_data)

Worst_data <- round(cancer_data[,c(22:31)], 2)
colnames(Worst_data)

## Correlation for all data


ggcorr(cancer_data[ ,c(-1)], name = "corr", label = TRUE) +
  theme(legend.position="none") +
  labs(title="Cancer Data")+
  theme(plot.title = element_text(face='bold',color='black',hjust=0.5,size=12))


## Correlation for Mean data 
ggcorr(mean_data, name = "corr", label = TRUE) +
  theme(legend.position="none")+
  labs(title="Cancer Mean Data")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))


## Correlation for SE data
ggcorr(SD_data, name = "corr", label = TRUE) +
  theme(legend.position="none")+
  labs(title="Cancer SE Data")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))

## Correlation for worst data
ggcorr(Worst_data, name = "corr", label = TRUE) +
  theme(legend.position="none")+
  labs(title="Cancer Worst Data")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))

## From the above correlation plot we can find that some of the attributes are highly correlated.







###########################
 ###  Preparing Data  ###
###########################

## Train and test data:
set.seed(165)
# from caret package
sample_set <- createDataPartition(cancer_data$diagnosis, times = 1, p = 0.7, list = F)
View(sample_set)

# Training set
Train_set <- cancer_data[sample_set,]
View(Train_set)
dim(Train_set) ## 399 31

# Testing set
Test_set <- cancer_data[-sample_set, ]
View(Test_set)
dim(Test_set) ## 170 31







###########################
###  Data modelling    ###
###########################

# Model 1

# Logistic Regression

model_LR <- train(diagnosis~., data = Train_set, method = "glm")

Pred_LR <- predict(model_LR, Test_set)

con.mat_LR <- confusionMatrix(Pred_LR, Test_set$diagnosis, positive = "M")
con.mat_LR 
# model accuracy 94.12

colnames(Train_set)


# Model 2

# Neural Networks
NN_Model1 <- nnet(Train_set$diagnosis~., data = Train_set, size = 10, linout = FALSE )

# Plot the N
plotnet(NN_Model1)

# Predict
Predict.results <-  predict(NN_Model1, Test_set[, -1], type = c('class'))
# Predict.results

acc <- sum(Test_set$diagnosis == Predict.results)/nrow(Test_set)
acc ## 88.82

    


# Model 3

# Decision Tree
library(rpart)
DT_Model <- rpart(diagnosis~., data=Train_set, control=rpart.control(minsplit=2))

DT_predict <- predict(DT_Model, Test_set[,-1], type="class")

DT_CM  <- confusionMatrix(DT_predict, Test_set$diagnosis)   
DT_CM # 92.94

# pruning the DT
CP_Value <- DT_Model$cptable[which.min(DT_Model$cptable[,"xerror"]),"CP"]

DT_Model_Prune <- prune(DT_Model, cp = CP_Value)

DT_predict_prune <- predict(DT_Model_Prune, Test_set[,-1], type="class")

DT_CM_Prune <-confusionMatrix(DT_predict_prune, Test_set$diagnosis)           
DT_CM_Prune # 92.35



# Model 4

# Random Forest

library(randomForest)

RF_Model <- randomForest(diagnosis ~., data=Train_set, ntree=500, proximity=T, importance=T)
RF_predict <- predict(RF_Model, Test_set[,-1])
RF_CM    <- confusionMatrix(RF_predict, Test_set$diagnosis)
RF_CM

# 95.88

# plot the RAndom Forest Error vs Trees.
plot(RF_Model, main= "Error Rate Vs Number of Tree Of Random Forest Model")


##############-----------------------#################
####             Feature selection


## correlation matrix
## By removing highly Correlated data.

# Finding Correlation between the independent variables
Cor_data <- cor(cancer_data[,c(-1)])

# Highly Correlated data.
high_Corr_data <- findCorrelation(Cor_data, cutoff = .85)
length(high_Corr_data)
filter_data <- cancer_data[, -high_Corr_data]
dim(filter_data)
# 569  18

filter_data$diagnosis <- cbind(as.character(cancer_data$diagnosis))
filter_data$diagnosis <- as.factor(filter_data$diagnosis)
class(filter_data$diagnosis)

# To print the selected Columns
colnames(filter_data)


# From Correlation Method for Feature Selection. 
# We have selected 18 attributes out off 30. 

set.seed(101)

# Partitioning the data
FS1_set <- createDataPartition(filter_data$diagnosis, times = 1, p = 0.7, list = F)

# New Training set
Train1_set <- filter_data[FS1_set,]

# New Testing set
Test1_set <- filter_data[-FS1_set, ]








# Now check the accuracy of the models after feature selection

## Model 1 -- Logistic Regression

CR_LR.model <- train(diagnosis ~., data = Train1_set, method = "glm")

CR_Pred_LR <- predict(CR_LR.model, Test1_set)

CR_con.mat_LR <- confusionMatrix(CR_Pred_LR, Test1_set$diagnosis, positive = "M")
CR_con.mat_LR 
# model accuracy 96.47






# Model 2 -- Neural Networks

# Neural Networks
CR_NN_Model1 <- nnet(Train1_set$diagnosis~., data = Train1_set, size = 10, linout = FALSE )

# Plot the N
plotnet(CR_NN_Model1)

CR_Predict.results <-  predict(CR_NN_Model1, Test1_set[, -19], type = c('class'))

CR_acc <- sum(Test1_set$diagnosis == CR_Predict.results)/nrow(Test1_set)
CR_acc
# 94.11







# Model 3 -- Decision Tree

# Decision Tree

CR_DT_Model <- rpart(diagnosis~., data = Train1_set, control=rpart.control(minsplit=2))

CR_DT_predict <- predict(CR_DT_Model, Test1_set[,-19], type="class")

CR_DT_CM  <- confusionMatrix(CR_DT_predict, Test1_set$diagnosis)   
CR_DT_CM # 92.35

# pruning the DT
CP_Value_CR <- CR_DT_Model$cptable[which.min(CR_DT_Model$cptable[,"xerror"]),"CP"]

DT_Model_Prune <- prune(CR_DT_Model, cp = CP_Value_CR)

DT_predict_prune <- predict(DT_Model_Prune, Test1_set[,-19], type="class")

CR_DT_CM_Prune <-confusionMatrix(DT_predict_prune, Test1_set$diagnosis)           
CR_DT_CM_Prune # 91.76





# Model 4

CR_RF_Model <- randomForest(diagnosis ~., data = Train1_set, ntree=500, proximity=T, importance=T)
CR_RF_predict <- predict(CR_RF_Model, Test1_set[,-19])
CR_RF_CM    <- confusionMatrix(CR_RF_predict, Test1_set$diagnosis)
CR_RF_CM
# 95.88


# plot the RAndom Forest Error vs Trees after Correlation Feature Selection.
plot(CR_RF_Model, main= "Error Rate Vs Number of Tree Of Random Forest Model")






## Method 2)
###  ------------------------ PCA method for feature selection

View(cancer_data) # - Complete data

# PCA does auto scaling

# PCA calculation
PCA_Res <- prcomp(cancer_data[,2:ncol(cancer_data)], center = TRUE, scale = TRUE)

# plot the variances
plot(PCA_Res, type="l", main = " Variance by Components Explanation")

install.packages("factoextra")
library(factoextra)



## Screen Plot
fviz_eig(PCA_Res, addlabels=TRUE, ylim=c(0,60), geom = c("bar", "line"), barfill = "pink", barcolor="grey",linecolor = "red", ncp=10)+
  labs(title = "PCA variance captured",
       x = "Principal Components", y = "% of variances")


# Collect the variables from PCA
Collect_var <- get_pca_var(PCA_Res)
Collect_var


# plot the correlation between the variables and PCA
library("corrplot")
corrplot(Collect_var$cos2, is.corr=FALSE)

# To get the mostly contributed variables to the Principle components
corrplot(Collect_var$contrib, is.corr=FALSE)  

# To check the cumulative proportion - the variance captured by the Principle Complonents.
summary(PCA_Res)


# BiPlot 
fviz_pca_biplot(PCA_Res, col.ind = cancer_data$diagnosis, col="black",
                palette = "jco", geom = "point", repel=TRUE,
                legend.title="Diagnosis", addEllipses = TRUE)

set.seed(111)

#  PCA Training and testing data
Train_pca <- predict(PCA_Res, Train_set)
Train_pca <- data.frame(Train_pca, Train_set[1])

Test_pca <- predict(PCA_Res, Test_set)
Test_pca <- data.frame(Test_pca, Test_set[1])



## Model 1 
## Logistic Regression using PCA 

LR_with_PCA <- train(diagnosis ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data = Train_pca, method = "glm")

LR_Pred_PCA <- predict(LR_with_PCA, Test_pca)

con.mat_LR <- confusionMatrix(LR_Pred_PCA, Test_pca$diagnosis, positive = "M")
con.mat_LR 
## 95.29%



# Model 2 
## Neural Networks using PCA
NN_PCA <- nnet(diagnosis ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data = Train_pca, size = 8, linout = FALSE )

# Plot the N
plotnet(NN_PCA)

# Predict
NN_Predict_PCA <-  predict(NN_PCA, Test_pca[,-31 ], type = c('class'))
# Predict.results

Acc_NN_PCA <- sum(Test_pca$diagnosis == NN_Predict_PCA)/nrow(Test_pca)
Acc_NN_PCA
# 96.47


# Model 3


# Decision Tree using PCA features.

DT_Model_PCA <- rpart(diagnosis~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data = Train_pca, control=rpart.control(minsplit=2))

DT_predict_PCA <- predict(DT_Model_PCA, Test_pca[ ,-31], type="class")

DT_CM_PCA  <- confusionMatrix(DT_predict_PCA, Test_pca$diagnosis)   
DT_CM_PCA # 95.29

# pruning the DT
CP_Value_PCA <- DT_Model_PCA$cptable[which.min(DT_Model_PCA$cptable[,"xerror"]),"CP"]

DT_Model_PCA_Prune <- prune(DT_Model_PCA, cp = CP_Value_PCA)

DT_predict_PCA_prune <- predict(DT_Model_PCA_Prune, Test_pca[ ,-31], type="class")

PCA_DT_CM_Prune <-confusionMatrix(DT_predict_PCA_prune, Test_pca$diagnosis)           
PCA_DT_CM_Prune  # 94.71






# Model 4

PCA_RF_Model <- randomForest(diagnosis ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data = Train_pca, ntree=500, proximity=T, importance=T)

PCA_RF_predict <- predict(PCA_RF_Model, Test_pca[,-31])

PCA_RF_CM    <- confusionMatrix(PCA_RF_predict, Test_pca$diagnosis)
PCA_RF_CM
# 94.71


# plot the RAndom Forest Error vs Trees after Correlation Feature Selection.
plot(PCA_RF_Model, main = "Error Rate Vs Number of Tree Of Random Forest Model")


























