install.packages(c("caret", 'modeest', 'crayon', 'tidyverse', 'dplyr', 'ggplot2', 'lattice', 'kernlab', 'e1071'))
install.packages('ggplot2')
install.packages("class")
library(caret)
library(dplyr)
library(modeest)
library(crayon)
library(tidyverse)
library(ggplot2)
library(lattice)
library(e1071)
library(kernlab)
getwd()
apartments <- read.csv('apartments_for_rent_classified_100K.csv', sep=';', header=TRUE)
summary(apartments)                
str(apartments)
head(apartments)

#Exploring dataset with ggplot
#Histogram of Prices
ggplot(apartments, aes(x = as.numeric(price))) +
  geom_histogram(binwidth = 100, fill = "blue", color = "black") +
  labs(title = "Distribution of Apartment Prices",
       x = "Price (USD)",
       y = "Frequency")

#Boxplot of Prices by Bedrooms
ggplot(apartments, aes(x = bedrooms, y = as.numeric(price))) +
  geom_boxplot(fill = "lightgreen", color = "darkgreen") +
  labs(title = "Prices Distribution by Number of Bedrooms",
       x = "Number of Bedrooms",
       y = "Price (USD)")

#Scatter Plot of Square Feet vs. Price
ggplot(apartments, aes(x = as.numeric(square_feet), y = as.numeric(price))) +
  geom_point(color = "orange") +
  labs(title = "Scatter Plot of Square Feet vs. Price",
       x = "Square Feet",
       y = "Price (USD)")
#Bar Chart of Apartment Categories
ggplot(apartments, aes(x = category)) +
  geom_bar(fill = "purple", color = "black") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Distribution of Apartment Categories",
       x = "Apartment Category",
       y = "Count")

#Remove Invalid Values
apartments <- na.omit(apartments)

#Processing and Pre-processing
library(dplyr)
#select appropriate columns for analysis
rent <- apartments %>% select(6:7, 10:12, 15, 17, 18) 
summary(rent)
str(rent)

#see all unique values from one column
unique_values <- unique(rent$pets_allowed)
unique_values
#count the occurrences of each unique value in a column to replace missing values with the most common 
count_values <- table(rent$pets_allowed)  
count_values

# convert to categories
rent$pets_allowed[rent$pets_allowed == "None"] <- 0
rent$pets_allowed[rent$pets_allowed == "Cats,Dogs"] <- 1
rent$pets_allowed[rent$pets_allowed == "Dogs"] <- 1
rent$pets_allowed[rent$pets_allowed == "Cats"] <- 1
rent$pets_allowed[rent$pets_allowed == "Cats,Dogs,None"] <- 1
rent$pets_allowed[rent$pets_allowed == "null"] <- 0

#Cleaning data
rent <- rent %>% filter(!pets_allowed %in% c(1242, 1252, 1299, 1325, 1545, 1601, 1622, 1900, 1930, 2305, 2375, 3010, 575))

# check the data type
str(rent) 
unique_values <- unique(rent$pets_allowed)
unique_values
rent$pets_allowed[rent$pets_allowed == "No"] <- 0
rent$pets_allowed[rent$pets_allowed == "Thumbnail"] <- 1
rent$pets_allowed[rent$pets_allowed == "Yes"] <- 1
rent$pets_allowed <- as.numeric(rent$pets_allowed)
#change data type
rent$pets_allowed <- as.numeric(rent$pets_allowed)
rent$bathrooms <- as.numeric(rent$bathrooms)
rent$bedrooms <- as.numeric(rent$bedrooms)
rent$price <- as.numeric(rent$price)
rent$square_feet <- as.numeric(rent$square_feet)
view(rent)
anyNA(rent)
na_counts <- colSums(is.na(rent))
#show the number of NAs for each column
na_counts
rent <- na.omit(rent)
anyNA(rent)

# Calculate mode
mode_pets <- as.integer(modeest::mfv(rent$pets_allowed))
mode_pets
rent$pets_allowed <- ifelse(rent$pets_allowed == 'NA', mode_pets, rent$pets_allowed)
table(rent$pets_allowed)
#view(rent)
str(rent)
unique_values2 <- unique(rent$bathrooms)
unique_values2
#delete rows with float numbers in the column of bathrooms
rent <- rent %>% filter(!bathrooms %in% c(1.5, 2.5, 3.5, 4.5, 5.5, 8.5))
mode_bath <- as.integer(modeest::mfv(rent$bathrooms))
# replacing missing values with mode
rent$bathrooms[is.na(rent$bathrooms)] <- mode_bath
table(rent$bathrooms)

table(rent$bedrooms)

table(rent$has_photo)
rent$has_photo[rent$has_photo == "No"] <- 0
rent$has_photo[rent$has_photo == "Thumbnail"] <- 1
rent$has_photo[rent$has_photo == "Yes"] <- 1
table(rent$has_photo)
rent$has_photo <- as.numeric(rent$has_photo)
view(rent)
str(rent)

# create categories of rental cost
# Define the breakpoints for categories
breaks <- c(-Inf, 1000, 2000, Inf) 

# Define the labels for the categories
labels <- c("Low", "Medium", "High") 
# Create a new column with the categories
rent$category <- cut(rent$price, breaks = breaks, labels = labels, include.lowest = TRUE)

# Convert specific columns to categorical
#rent <- as.data.frame(lapply(rent, as.factor))
columns_to_convert <- c("cityname", "state", "category")
#define a function to convert columns to factors without changing values
convert_to_factor <- function(x) {
  if(is.numeric(x) | is.character(x)) {
    as.factor(x)
  } else {
    x
  }
}

# Apply the custom function to specified columns
rent[, columns_to_convert] <- lapply(rent[, columns_to_convert], convert_to_factor)
str(rent)
view(rent)
rent$has_photo <- as.numeric(as.character(rent$has_photo))
rent$pets_allowed <- as.numeric(as.character(rent$pets_allowed))
rent_numeric <- as.data.frame(lapply(rent, as.numeric))
#view(rent_numeric)
#remove the column of actual price
rent_num <- rent_numeric %>% select(1:4, 6:9)
view(rent_num)

#check the column of square feet for outliers
boxplot(rent$square_feet, main="Boxplot of Square Feet")
# Calculate summary statistics
summary_stats <- summary(rent_num$square_feet)
print(summary_stats)
#Delete rows where 'square_feet' is less than 100
rent_num <- rent_num[rent_num$square_feet >= 100, ]
# Identify potential outliers using the IQR method
#rent_num$square_feet <- as.numeric(as.character(rent_num$square_feet))
#Q1 <- quantile(rent_num$square_feet, 0.25)
#Q3 <- quantile(rent_num$square_feet, 0.75)
#IQR <- Q3 - Q1
#lower_bound <- Q1 - 1.5 * IQR
#upper_bound <- Q3 + 1.5 * IQR

#potential_outliers <- rent_num$square_feet[rent_num$square_feet < lower_bound | rent_num$square_feet > upper_bound]
# Display potential outliers
#if (length(potential_outliers) > 0) {
#  cat("Potential outliers:\n")
#  print(potential_outliers)
#} else {
#  cat("No potential outliers found.\n")
#}

#Checking and comleting missing values
missing_rent <- sum(!complete.cases(rent_num))
cat("Number of missing values in dataset:", missing_rent, "\n")

# Remove rows with missing values
#rent_num <- rent_num[complete.cases(rent_num), ]

correlation_matrix <- cor(rent_num)
ggplot(data = as.data.frame(as.table(correlation_matrix)), 
       aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0, limits = c(-1, 1)) +
  theme_minimal() +
  labs(title = "Correlation Plot")

#barplot(table(rent_num$bathrooms), main = "Distribution of Bathrooms")
#barplot(table(rent_num$bedrooms), main = "Distribution of Bedrooms")

# Data Normalization
head(rent_num)
normalize <- function(x) {
  return((x-min(x))/(max(x) - min(x))) }
#normalize each column separately 
#do not normalize the target feature of price category
rent_norma <- rent_num[, c(1:7)]
rent_norma <- as.data.frame(lapply(rent_norma, scale))
head(rent_norma)
rent_norma$category <- rent_num$category
rent_norma

# Data Splicing
set.seed(123)
data <- sample(1:nrow(rent_norma), size=nrow(rent_norma)*0.7, replace = FALSE)
train.rent <- rent_norma[data,]
test.rent <- rent_norma[-data,]
#Creating separate dataframe for 'category' feature which is our target
train.rent_labels <- rent_norma[data,8] 
test.rent_labels <- rent_norma[-data,8]

library(class)

#Find the number of observation
NROW(train.rent_labels)
result <- sqrt(34741)
result

knn.186 <- knn(train=train.rent, test=test.rent, cl=train.rent_labels, k=186)
knn.187 <- knn(train=train.rent, test=test.rent, cl=train.rent_labels, k=187)
knn.3 <- knn(train=train.rent, test=test.rent, cl=train.rent_labels, k=3)
#Model Evaluation
#Calculate the proportion of correct classification for k = 186, 187
ACC.186 <- 100 * sum(test.rent_labels == knn.186)/NROW(test.rent_labels)
ACC.187 <- 100 * sum(test.rent_labels == knn.187)/NROW(test.rent_labels)
ACC.3 <- 100 * sum(test.rent_labels == knn.3)/NROW(test.rent_labels)
ACC.186
ACC.187
ACC.3
# Check prediction against actual value in tabular form for k=186
table(knn.186 ,test.rent_labels)
knn.186

# Check prediction against actual value in tabular form for k=187
table(knn.187,test.rent_labels)

table(knn.3 ,test.rent_labels)
library(caret)
library(ggplot2)
confusionMatrix(table(knn.187 ,test.rent_labels))
cm <- confusionMatrix(table(knn.187, test.rent_labels))
conf_matrix <- as.matrix(cm)
# Convert to data frame for ggplot
conf_df <- as.data.frame(conf_matrix)
conf_df$Actual <- rownames(conf_df)

# Reshape the data for ggplot
conf_df_long <- tidyr::gather(conf_df, Predicted, Count, -Actual)

# Create a ggplot bar chart
ggplot(conf_df_long, aes(x = Actual, y = Count, fill = Predicted)) +
  geom_bar(stat = "identity") +
  labs(title = "Confusion Matrix", x = "Actual", y = "Count") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

#Optimization
i=1
k.optm=1
for (i in 1:187){ knn.mod <- knn(train=train.rent, test=test.rent, cl=train.rent_labels, k=i)
k.optm[i] <- 100 * sum(test.rent_labels == knn.mod)/NROW(test.rent_labels)
k=i
cat(k,'=',k.optm[i],'')}

#The accuracy for k=1 is the best: 1 = 99.56347
#The accuracy for k=187 is 92.69308
#The accuracy for k=3 is 99.43586.

#represent accuracy graphically
plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level")

#Statictical tests
# Convert factors to numeric
actual_values <- as.numeric(as.character(test.rent_labels))
predicted_values <- as.numeric(as.character(knn.187))
# calculate the Mean Squared Error for test results
mse <- mean((actual_values - predicted_values)^2)

# Print the MSE
cat("Mean Squared Error:", mse)

install.packages("pROC")
install.packages("ROCR")
library(pROC)
library(ROCR)
# Create an ROC curve
actual_values <- as.numeric(as.character(test.rent_labels))
predicted_values <- as.numeric(as.character(knn.3))

roc_curve <- roc(ifelse(actual_values == 2, 1, 0), predicted_values)
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)

# Calculate AUC (Area Under the Curve)
auc_value <- auc(roc_curve)
cat("AUC:", auc_value, "\n")

# Print the ROC curve details
print(roc_curve)

# Combine actual and predicted values into a dataframe
distribution_data <- data.frame(Category = c(rep("Actual", length(actual_values)), rep("Predicted", length(predicted_values))),
                                Value = c(actual_values, predicted_values))

# Create a bar plot for class distribution
class_distribution_plot <- ggplot(distribution_data, aes(x = Category, fill = as.factor(Value))) +
  geom_bar(position = "dodge") +
  labs(title = "Actual vs. Predicted Class Distribution", x = "Category", y = "Count") +
  scale_fill_manual(values = c("Actual" = "blue", "Predicted" = "red")) +  # Adjust colors as needed
  theme_minimal()

# Display the plot
print(class_distribution_plot)
