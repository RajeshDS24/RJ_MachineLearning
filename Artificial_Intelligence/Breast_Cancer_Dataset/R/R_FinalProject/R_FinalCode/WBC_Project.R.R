####################################################### 
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
##                  
##                  
##                  
##                  
##                  
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#######################################################



## Installing Required R packages
install.packages("ggplot2") # For data visualization 
install.packages("dplyr") # For Data manipulation
install.packages("caret") # for data modeling
install.packages("NeuralNetTools") # For Neural Networks
install.packages("funModeling") # For Histograms visio
install.packages("randomForest")  # Random FOrest Modeling
install.packages("e1071")
install.packages("rpart")
install.packages("corrplot")
install.packages("RColorBrewer")
install.packages("factoextra")
install.packages("nnet")


# Adding libraries
library(dplyr)
library(ggplot2)
library(caret)
library(NeuralNetTools)
library(funModeling)
library(randomForest)
library(e1071) 
library(rpart)
library(corrplot)
library(RColorBrewer)
library(factoextra)
library(nnet)



# Set the path to load the data to R
getwd()

WBC_Data.df <- read.csv("data.csv") %>% na.omit()

# To see the whole data set
View(WBC_Data.df)

## From CRISP-DM framework
### Data understanding
colnames(WBC_Data.df)

# Find number of classes in the output variables.
class(WBC_Data.df$diagnosis)

# to check the Rows and Columns:
dim(WBC_Data.df) 

# To check the data type and data format
str(WBC_Data.df)

## check null values
is.null(WBC_Data.df)  ## returns boolean

which(is.na(WBC_Data.df))  ## Returns int

# 
table(WBC_Data.df$diagnosis)

# Percentage on the classes.
prop.table(table(WBC_Data.df$diagnosis))


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ---- Data Visualization 



## CRISP-DM FrameWork
## ~~~~~~~~~~~~~~~~~~~  Data Preparation

## the data set has unwanted columns so removing them 
WBC_Data <- WBC_Data.df[c(-1,-33)]
colnames(WBC_Data)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# From funModeling package
plot_num(WBC_Data.df)


# Collecting the Mean data.
Mean_WBC <- WBC_Data[, c(2:11)]
colnames(Mean_WBC)

SE_WBC <- WBC_Data[, c(12:21)]
colnames(SE_WBC)

Worst_WBC <- WBC_Data[, c(22:31)]
colnames(Worst_WBC)

# Histograms for these three groups of data.
# For Mean WBC data
plot_num(Mean_WBC)

# For Standard Error WBC data
plot_num(SE_WBC)

# For Worst WBC data 
plot_num(Worst_WBC)


## pearson Correlation

## For easy visualization divide the data into three groups:
## namely -- Mean, Standard Error and Worst.


Corr_matrix_mean <-cor(Mean_WBC)
corrplot(Corr_matrix_mean, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))

Corr_matrix_SE <-cor(SE_WBC)
corrplot(Corr_matrix_SE, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlGn"))

Corr_matrix_Worst <-cor(Worst_WBC)
corrplot(Corr_matrix_Worst, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))


Corr_matrix <-cor(WBC_Data[,c(2:31)])
corrplot(Corr_matrix, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))



# Check the structure again
str(WBC_Data)

## Seems like diagnosis is char here. so change to factor.
WBC_Data$diagnosis <- as.factor(WBC_Data$diagnosi)


## set seed value so that when ever we run the model we can take same Train and test data:
set.seed(143)

# Creating the partition for training and testing
Partition_data <- createDataPartition(WBC_Data$diagnosis, times = 1, p = 0.75, list = F)
head(Partition_data)

# Train data
Train_Data <- WBC_Data[Partition_data,]


# Test data
Test_Data <- WBC_Data[-Partition_data, ]


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##     Predictive Modeling with raw data:
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~ Random Forest --- (sai kumar)

RF.model <- randomForest(diagnosis ~., Train_Data, ntree = 500, 
                         importance=TRUE)

RF.Predict <- predict(RF.model, Test_Data)
RF.CM <- confusionMatrix(RF.Predict, Test_Data$diagnosis, positive = "M")
RF.CM

# ~~~  Decision Tree -- (Vajratiya Vajrobol - 33296085)

DT.model <- rpart(diagnosis ~., Train_Data, control=rpart.control(minsplit=2))

DT.Predict <- predict(DT.model, Test_Data, type="class")
DT.CM <- confusionMatrix(DT.Predict, Test_Data$diagnosis, positive = "M")
DT.CM # 95.07

# pruning the DT
CP <- DT.model$cptable[which.min(DT.model$cptable[,"xerror"]),"CP"]

DT.Model.Prune <- prune(DT.model, cp = CP)

DT.predict.prune <- predict(DT.Model.Prune, Test_Data[,-1], type="class")

DT.CM.Prune <-confusionMatrix(DT.predict.prune, Test_Data$diagnosis)           
DT.CM.Prune # 95.07


##  ~~ Neural Net -- (Rajesh - 33669079)
NN_Model <- train(diagnosis ~.,Train_Data, method = "nnet")

NN.pred <- predict(NN_Model, Test_Data)

NN.CM <- confusionMatrix(NN.pred, Test_Data$diagnosis, positive = "M")
NN.CM

## Neural network structure for complete data.
plotnet(NN_Model)


## ------------------------------------------ 
# Normalizing data before further process

#define normalization function

Norm_fun <- function(x) 
  {
  (x - min(x)) / (max(x) - min(x))
}

# Apply Norm_fun on the whole data.
WBC_Data[2:31] <- as.data.frame(lapply(WBC_Data[2:31], Norm_fun))

# View the normalized data.
View(WBC_Data)






# -------------------------------------------------- 

## Feature selection using Filter Method
##    1) Correlation

## Feature Selection Using Embedded Method
##    1) Random Forest variable selection

# --------------------------------------------------

# Feature Selection 1
# using Filter Method
# 1) ---- Correlation 
WBC_corr <- cor(WBC_Data[,c(-1)])

# Finding the correlated data
WBC_Corr_data <- findCorrelation(WBC_corr, cutoff = .80)
length(WBC_Corr_data) ## 16 

WBC_selected <- WBC_Data[, -WBC_Corr_data]
WBC_selected
dim(WBC_selected)
# 569  15

# Adding the diagnosis column to the selected data.
WBC_selected$diagnosis <- cbind(as.character(WBC_Data$diagnosis))

# converting diagnosis to factor
WBC_selected$diagnosis <- as.factor(WBC_selected$diagnosis)
class(WBC_selected$diagnosis)

# To print the selected Columns
colnames(WBC_selected)

set.seed(545)

### ---- Data partition after feature selection 1.
Corr_set <- createDataPartition(WBC_selected$diagnosis, times = 1, p = 0.75, list = F)

# Train data after feature selection 
Corr_train <- WBC_selected[Corr_set,]


# Test data
Corr_test <- WBC_selected[-Corr_set, ]


### ---- Data modeling after feature selection 1.

# ~~ Random Forest --- (sai kumar)

RF.corr.model <- randomForest(diagnosis ~., Corr_train, ntree = 500, 
                         importance=TRUE, proximity=T)

RF.corr.Predict <- predict(RF.corr.model, Corr_test[,c(1:15)])
RF.corr.CM <- confusionMatrix(RF.corr.Predict, Corr_test$diagnosis, positive = "M")
RF.corr.CM # 0.95 



# ~~~  Decision Tree -- (Vajratiya Vajrobol - 33296085)

DT.corr.model <- rpart(diagnosis ~., Corr_train, control = rpart.control(minsplit=2))

DT.Corr.Predict <- predict(DT.corr.model, Corr_test, type="class")
DT.Corr.CM <- confusionMatrix(DT.Corr.Predict, Corr_test$diagnosis, positive = "M")
DT.Corr.CM # 0.9366

# pruning the DT
CP.Corr <- DT.corr.model$cptable[which.min(DT.corr.model$cptable[,"xerror"]),"CP"]

DT.corr.model.Prune <- prune(DT.corr.model, cp = CP.Corr)

DT.Corr.Predict.prune <- predict(DT.corr.model.Prune, Corr_test[,-16], type="class")

DT.CM.Prune <-confusionMatrix(DT.Corr.Predict.prune, Corr_test$diagnosis, positive = "M")           
DT.CM.Prune #93.66 -- no difference


## Neural Networks 

NN.Corr.Model <- nnet(diagnosis ~.,Corr_train, size = 10, linout = FALSE) # linout is True for linear output

NN.Corr.pred <- predict(NN.Corr.Model, Corr_test[,-16], type = "class")

NN.Corr.CM <- confusionMatrix(as.factor(NN.Corr.pred), Corr_test$diagnosis, positive = "M")
NN.Corr.CM # 0.9437         


## Neural network structure
plotnet(NN.Corr.Model)

## ----------------------------------------------------------

# Feature Selection 2
# Using the Embedded Method
## 1)  Random Forest variable selection

# Using complete data set again.
RF_Features <- randomForest(diagnosis ~., data = Train_Data, ntree = 500, 
                            importance=TRUE)

RF_Features_Sel <- predict(RF_Features, Test_Data)

# compare the feature importance with varImp() function
varImp(RF_Features_Sel)

# Create a plot of importance scores by random forest
varImpPlot(RF_Features_Sel)

importance    <- importance(RF_Features)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

#Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('',dense_rank(desc(Importance))))

#Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() 


sel_list <- rankImportance %>% arrange(desc(Importance)) %>% head(10)
sel_list


# Select only these columns from the original data set.

RF_feature_selection <- WBC_Data %>% select(diagnosis, perimeter_worst,      
                                            concave.points_worst ,
                                            concave.points_mean,  
                                            area_worst ,         
                                            radius_worst,        
                                            area_mean  ,          
                                            concavity_mean ,      
                                            perimeter_mean ,     
                                            radius_mean,         
                                            concavity_worst)

colnames(RF_feature_selection)
dim(RF_feature_selection)
class(RF_feature_selection)

set.seed(657)

# Partitioning the data.
RF_FS_part <- createDataPartition(RF_feature_selection$diagnosis, times = 1, p = 0.75, list = F)
head(RF_FS_part)

# Train data
RF_Train <- RF_feature_selection[RF_FS_part,]
dim(RF_Train)

# Test data
RF_Test <- RF_feature_selection[-RF_FS_part, ]
dim(RF_Test)

## Design the models using the Random Forest feature selection.
### ---- Data modeling after feature selection 2.

# ~~ Random Forest --- (sai kumar)

RF.RFFS.model <- randomForest(diagnosis ~., RF_Train, ntree = 500, 
                              importance=TRUE, proximity=T)

RF.RFFS.Predict <- predict(RF.RFFS.model, RF_Test[,c(2:11)])
RF.RFFS.CM <- confusionMatrix(RF.RFFS.Predict, RF_Test$diagnosis, positive = "M")
RF.RFFS.CM # 95.07



# ~~~  Decision Tree -- (Vajratiya Vajrobol - 33296085)

DT.RFFS.model <- rpart(diagnosis ~., RF_Train, control = rpart.control(minsplit=2))

DT.RFFS.Predict <- predict(DT.RFFS.model, RF_Test[, c(2:11)], type="class")
DT.RFFS.CM <- confusionMatrix(DT.RFFS.Predict, RF_Test$diagnosis, positive = "M")
DT.RFFS.CM # 0.9437 

# pruning the DT
CP.RFFS <- DT.RFFS.model$cptable[which.min(DT.RFFS.model$cptable[,"xerror"]),"CP"]

DT.RRFS.model.Prune <- prune(DT.RFFS.model, cp = CP.RFFS)

DT.RFFS.Predict.prune <- predict(DT.RRFS.model.Prune, RF_Test[,-16], type="class")

DT.RFFS.CM.Prune <-confusionMatrix(DT.RFFS.Predict.prune, RF_Test$diagnosis, positive = "M")           
DT.RFFS.CM.Prune # 0.9366 



## Neural Networks 

NN.RRFS.Model <- nnet(diagnosis ~.,RF_Train, size = 10, linout = FALSE) # linout is True for linear output

NN.RFFS.pred <- predict(NN.RRFS.Model, RF_Test[,c(2:11)], type = "class")

NN.RFFS.CM <- confusionMatrix(as.factor(NN.RFFS.pred), RF_Test$diagnosis, positive = "M")
NN.RFFS.CM # 0.9296         


plotnet(NN.RRFS.Model)

## -----------------------------------
## dimensionality reduction using Principle COmponent Analysis 
## -----------------------------------

## PCA used for dimensionality reduction.

View(WBC_Data) 


# PCA on raw data.
WBC_PCA <- prcomp(WBC_Data[,2:ncol(WBC_Data)], center = TRUE, scale = T)

# The Variance plot 
plot(WBC_PCA, type="l", main = " Explaining the variance in the PCA ")




## Screen Plot
fviz_eig(WBC_PCA, addlabels=TRUE, ylim=c(0,60), geom = c("bar", "line"), 
        barfill = "pink", barcolor="grey",linecolor = "red", ncp=10) +
  labs(title = "Variance captured by PCA components",
       x = "Number of Principal Components", y = "Variance captured in percentage ")


# Collect the variables from PCA
WBC_var_PCA <- get_pca_var(WBC_PCA)
WBC_var_PCA



# Check he variance captured by each principle component.
summary(WBC_PCA)


# BiPlot 
fviz_pca_biplot(WBC_PCA, col.ind = WBC_Data$diagnosis, col="black",
                palette = "jco", geom = "point", repel=TRUE,
                legend.title="Diagnosis", addEllipses = TRUE)

set.seed(145)

WBC.PCA1_6 <- kmeans(WBC_var_PCA$coord, centers = 6, nstart = 25)
Grp <- as.factor(WBC.PCA1_6$cluster)

fviz_pca_var(WBC_PCA, col.var = Grp, 
             palette = "jco",
             legend.title = "Cluster")



set.seed(105)


# PCA training and testing sets.
PCA.train <- predict(WBC_PCA, Train_Data)
PCA.train <- data.frame(PCA.train, Train_Data[1])
PCA.train

PCA_test <- predict(WBC_PCA, Test_Data)
PCA_test <- data.frame(PCA_test, Test_Data[1])
PCA_test



## Building the models using using PCA components.
attach(PCA.train)

# ~~ Random Forest --- (sai kumar)

formula <- 

RF.PCA.model <- randomForest(diagnosis ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + 
                               PC7 + PC8 + PC9 + PC10 + PC11 + PC12, PCA.train, ntree = 500, 
                              importance=TRUE, proximity=T)

RF.PCA.Predict <- predict(RF.PCA.model, PCA_test[,c(1:12)])
RF.PCA.CM <- confusionMatrix(RF.PCA.Predict, PCA_test$diagnosis, positive = "M")
RF.PCA.CM # 94.37



# ~~~  Decision Tree -- (Vajratiya Vajrobol - 33296085)

DT.PCA.model <- rpart(diagnosis ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + 
                         PC7 + PC8 + PC9 + PC10 + PC11 + PC12, 
                       data = PCA.train, control = rpart.control(minsplit=2))

DT.PCA.Predict <- predict(DT.PCA.model, PCA_test[, c(1:12)], type="class")
DT.PCA.CM <- confusionMatrix(DT.PCA.Predict, PCA_test$diagnosis, positive = "M")
DT.PCA.CM # 92.96

# pruning the DT
CP.PCA <- DT.PCA.model$cptable[which.min(DT.PCA.model$cptable[,"xerror"]),"CP"]

DT.PCA.model.Prune <- prune(DT.PCA.model, cp = CP.PCA)

DT.PCA.Predict.prune <- predict(DT.PCA.model.Prune, PCA_test[,c(1:12)], type="class")

DT.PCA.CM.Prune <-confusionMatrix(DT.PCA.Predict.prune, PCA_test$diagnosis, positive = "M")           
DT.PCA.CM.Prune # 92.96


## Neural Networks 

NN.PCA.Model <- nnet(diagnosis ~  PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + 
                        PC7 + PC8 + PC9 + PC10 + PC11 + PC12, 
                      PCA.train, size = 10, linout = FALSE) 

NN.PCA.pred <- predict(NN.PCA.Model, PCA_test[,c(1:12)], type = "class")

NN.PCA.CM <- confusionMatrix(as.factor(NN.PCA.pred), PCA_test$diagnosis, positive = "M")
NN.PCA.CM # 95.07

plotnet(NN.PCA.Model)

























