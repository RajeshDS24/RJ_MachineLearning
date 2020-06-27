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

# --- or 

# Percentage of missing data
gg_miss_var(MASS_data, show_pct = TRUE)

# --- or

## Visualizing the missing values using VIM package
aggr(MASS_data, prop = F, numbers = T)


scattmatrixMiss(MASS_data, interactive = F, highlight = c("MASS_data"))

# visualizing null values in r -- Rajesh.
missing_data <- miss_var_summary(MASS_data) 
ggplot(missing_data, aes(x = missing_data$variable, y = missing_data$n_miss, fill = missing_data$variable)) + 
  geom_bar(stat="identity") + scale_fill_brewer(palette="RdPu") + 
  labs(title = "Displaying Missing Data Values in Bar chart", x = "Factors effecting the Breast Cancer", y = "Number of missing values")


# As the missing values are randomly distributed. we shall drop the missing values.
# Omitting the missing values in the dataset.
MASS_CleanData <- MASS_data %>% na.omit()
View(MASS_CleanData)


# ??normalize
# Norm_data <- normalize(MASS_CleanData, method = "standardize", range = c(0, 1), margin = 1L, on.constant = "quiet")


##### normalize function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

MASS_norm <- MASS_CleanData[ ,c(2,3,4,5,6) ]

colnames(MASS_CleanData)
colnames(MASS_norm)

# norm <- as.data.frame(lapply(MASS_input.norm, normalize))

MASS_norm$Age <- normalize(MASS_norm$Age)
MASS_norm$Shape <- normalize(MASS_norm$Shape)
MASS_norm$Margin <- normalize(MASS_norm$Margin)
MASS_norm$Density <- normalize(MASS_norm$Density)


View(MASS_norm)

## Data partition
Data_split.norm <- createDataPartition(MASS_norm$Severity, times = 1, p = 0.7, list = F)









