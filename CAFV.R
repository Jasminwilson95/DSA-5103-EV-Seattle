
# Load the library
{
  library(dplyr)
  library(forcats)
  library(tidyverse)
  library(ggplot2)
  library(car)
  library(plotly)
  library(viridis)
  library(corrplot) #correlations 
  library(caret)
  library(glmnet)
  library(MASS)
  library(randomForest) # for random forest model
  library(e1071) # For SVM model
  library(earth) #for Mars
}

#Loading the dataset:
ev.data <- read.csv('Electric_Vehicle_Population_Data.csv',na.strings = c("", "NA"))


###### 1) Exploratory Data Analysis (EDA) ######

str(ev.data)
# Summary of the dataset
summary(ev.data)

#Check & list variables that are numeric, factor, character
ev.data %>% select_if(is.numeric) %>% names() # 7 numeric
ev.data %>% select_if(is.factor) %>% names() # 0 factor
ev.data %>% select_if(is.character) %>% names() # 10 character



###Exploring Distributions of Categorical Variables
# Distribution of 'Make'
ggplot(ev.data, aes(x=Make)) + 
  geom_bar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Distribution of Car Makes", x = "Make", y = "Count")

# Distribution of 'Model' (top 20 models)
ggplot(ev.data %>% group_by(Model) %>% 
         summarize(Count = n()) %>% 
         top_n(20, Count), aes(x=reorder(Model, Count), y=Count)) + 
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Top 20 Car Models", x = "Model", y = "Count")

# Distribution of 'EV Type'
ggplot(ev.data, aes(x=Electric.Vehicle.Type)) + 
  geom_bar() + 
  labs(title = "Distribution of Electric Vehicle Types",
       x = "Electric Vehicle Type", y = "Count")

###Numerical Data Analysis
# Histogram of 'Model Year'
ggplot(ev.data, aes(x=Model.Year)) + 
  geom_histogram(bins=30, fill="blue", color="orange") +
  labs(title = "Distribution of Model Year", x = "Model Year", y = "Count")

# Histogram of 'Electric Range'
ggplot(ev.data, aes(x=Electric.Range)) + 
  geom_histogram(bins=30, fill="green", color="navy") +
  labs(title = "Distribution of Electric Range", x = "Electric Range", y = "Count")

# Histogram of 'Base MSRP' (ignoring zero values for Base.MSRP)
ggplot(ev.data %>% filter(Base.MSRP > 0), aes(x=Base.MSRP)) + 
  geom_histogram(bins=30, fill="orange", color="red") +
  labs(title = "Distribution of Base MSRP", x = "Base MSRP", y = "Count")


#Handling Missing Values
# Imputing missing values for categorical variables
ev.data$County[is.na(ev.data$County)] <- names(which.max(table(ev.data$County)))
ev.data$City[is.na(ev.data$City)] <- names(which.max(table(ev.data$City)))
ev.data$Electric.Utility[is.na(ev.data$Electric.Utility)] <- names(which.max(table(ev.data$Electric.Utility)))

# Imputing missing values for numerical variables
ev.data$Postal.Code[is.na(ev.data$Postal.Code)] <- median(ev.data$Postal.Code, na.rm = TRUE)
ev.data$Legislative.District[is.na(ev.data$Legislative.District)] <- median(ev.data$Legislative.District, na.rm = TRUE)
ev.data$X2020.Census.Tract[is.na(ev.data$X2020.Census.Tract)] <- median(ev.data$X2020.Census.Tract, na.rm = TRUE)

# Remove EV from outside Washington state:
ev.data <- ev.data %>% 
  filter(State == "WA")

# Checking for missing values
sum(is.na(ev.data))
ev.data <- na.omit(ev.data)  # To remove the remaining missing values 


## Correlation Analysis  ####
# Numerical 
numerical_columns <- c("Model.Year", "Electric.Range", "Base.MSRP", "Postal.Code",
                       "Legislative.District", "X2020.Census.Tract")
correlation_matrix <- cor(ev.data[numerical_columns], use="complete.obs")

cor(correlation_matrix, use = "complete.obs")
summary(correlation_matrix)
# Plot correlation heatmap
corrplot(correlation_matrix, method = "color")


### 2) CAFV Eligibility ####
# Distribution of CAFV eligibility
ggplot(ev.data, aes(x=Clean.Alternative.Fuel.Vehicle..CAFV..Eligibility)) + 
  geom_bar(fill="gray", color="navy") + 
  labs(title = "Distribution of CAFV Eligibility", x = "CAFV Eligibility", y = "Count")

# Exploring relationship with the Type of Electric Vehicle 
ggplot(ev.data, aes(x=Electric.Vehicle.Type, fill=Clean.Alternative.Fuel.Vehicle..CAFV..Eligibility)) + 
  geom_bar(position="dodge") + 
  labs(title = "CAFV Eligibility by Electric Vehicle Type", x = "Electric Vehicle Type", y = "Count")

# Boxplots for Electric Range and Base MSRP by CAFV Eligibility
ggplot(ev.data, aes(x=Clean.Alternative.Fuel.Vehicle..CAFV..Eligibility, y = Electric.Range)) + 
  geom_boxplot(fill="gray",color="navy") + 
  labs(title = "CAFV Eligibility by Electric Range", x = "CAFV Eligibility", y = "Electric Range")

ggplot(ev.data, aes(x=Clean.Alternative.Fuel.Vehicle..CAFV..Eligibility, y = Base.MSRP)) + 
  geom_boxplot() + 
  labs(title = "Base MSRP by CAFV Eligibility", x = "CAFV Eligibility", y = "Base MSRP")

#Predictive Modeling

# Preparing the data
ev.data$CAFV <- as.factor(ev.data$Clean.Alternative.Fuel.Vehicle..CAFV..Eligibility)
ev.model.data <- ev.data[, c("Electric.Vehicle.Type", "Electric.Range", "Base.MSRP", "CAFV")]

# Now we can split data into training and testing sets
set.seed(123)  
trainIndex <- createDataPartition(ev.model.data$CAFV, 
                                  p = .8, list = FALSE, times = 1)
trainData <- ev.model.data[trainIndex, ]
testData <- ev.model.data[-trainIndex, ]

# Random Forest Model
rf.model <- randomForest(CAFV ~ ., data=trainData)
prediction <- predict(rf.model, testData)

# Model Evaluation
confusionMatrix(prediction, testData$CAFV)

#Evaluation show an accuracy of 1 (100%), with a Kappa value of 1. 
#This indicates a perfect classification by the model, which is quite unusual 
#in real-world scenarios. This suggests that the model might be overfitting. 


#Trying Different Models:
# Preparing data for modeling (using ev.model.data)
set.seed(123)

# Train multiple models using cross-validation
control <- trainControl(method="cv", number=2)
metric <- "Accuracy"

#1) Random Forest Model
rf_model <- train(CAFV ~ ., data=ev.model.data,
                  method="rf",
                  trControl=control,
                  metric=metric)

#2) Support Vector Machine Model
svm_model <- train(CAFV ~ ., data=ev.model.data, 
                   method="svmRadial", 
                   trControl=control,
                   metric=metric)
#3)  Decision Tree
treeModel <- train(CAFV ~ ., data=trainData, method="rpart", trControl=control)

#4) Gradient Boosting Machine
gbmModel <- train(CAFV ~ ., data=trainData, method="gbm", trControl=control, verbose=FALSE)

#5)  LASSO Model
lassoModel <- train(CAFV ~ ., data=trainData, method="glmnet", trControl=control, tuneLength=10)
 
#6) Ridge Regression
ridgeModel <- train(CAFV ~ ., data=trainData, method="glmnet", trControl=control, tuneLength=10, tuneGrid=expand.grid(alpha=0, lambda=seq(0.001, 0.1, length=10)))

#7) Multivariate Adaptive Regression Splines (MARS)
marsModel <- train(CAFV ~ ., data=trainData, method="earth", trControl=control)

#8)Linear Discriminant Analysis (LDA)
ldaModel <- train(CAFV ~ ., data=trainData, method="lda", trControl=control)



# Comparing model performances
results <- resamples(list(RandomForest=rf_model,
                          SVM=svm_model, 
                          DecisionTree=treeModel,
                          GradientBoosting= gbmModel,
                          LASSO=lassoModel,
                          Ridge=ridgeModel,
                          MARS=marsModel,
                          LDA=ldaModel
                          ))

summary(results)

#Random Forest, Decision Tree, Gradient Boosting, MARS: These models show perfect accuracy (1.000) and Kappa (1.000),
     #which is highly unusual in practice. These models might be overfitting. We may eliminate those models 

#SVM, LASSO, Ridge Regression: Shows very high accuracy (>0.95) and Kappa (0.97), indicating excellent performance for these models.
#LDA: Exhibits the lowest accuracy (0.832) and Kappa (0.729), indicating lowest performance that it might not be as suitable. But could be good fit for stacked model 


## Hyperparameter Tuning ## Optional - 
# #1. SVM 
# set.seed(123)
# svmGrid <- expand.grid(C = seq(0.1, 1, length = 10),
#                        sigma = seq(0.1, 1, length = 10))
# svmTuned <- train(CAFV ~ ., data=trainData, method="svmRadial",
#                   trControl=control, tuneGrid=svmGrid)
# 
# #2. LASSO and Ridge Regression
# # LASSO
# lassoGrid <- expand.grid(alpha = 1, lambda = seq(0.001, 0.1, length = 10))
# lassoTuned <- train(CAFV ~ ., data=trainData, method="glmnet",
#                     trControl=control, tuneGrid=lassoGrid)
# 
# # Ridge
# ridgeGrid <- expand.grid(alpha = 0, lambda = seq(0.001, 0.1, length = 10))
# ridgeTuned <- train(CAFV ~ ., data=trainData, method="glmnet",
#                     trControl=control, tuneGrid=ridgeGrid)
# 
# #3. Linear Discriminant Analysis (LDA) # No need for Tunning 
# ldaModel <- train(CAFV ~ ., data=trainData, method="lda", trControl=control)



#### Model Prediction and Model Evaluation####
#1) SVM
svmPredictions <- predict(svm_model, testData)
confMatrixSVM <- confusionMatrix(svmPredictions, testData$CAFV)
print(confMatrixSVM)
#2) LASSO Model
lassoPredictions <- predict(lassoModel, testData)
confMatrixLASSO <- confusionMatrix(lassoPredictions, testData$CAFV)
print(confMatrixLASSO)
#3) Ridge Regression
ridgePredictions <- predict(ridgeModel, testData)
confMatrixRidge <- confusionMatrix(ridgePredictions, testData$CAFV)
print(confMatrixRidge)
#4) LDA                       <- # This model can be dropped for its lower accuracy & lower Kappa
ldaPredictions <- predict(ldaModel, testData)
confMatrixlda <- confusionMatrix(ldaPredictions, testData$CAFV)
print(confMatrixlda)

##Model Evaluation Summary Table##
summary_table <- data.frame(
  Metric = rep(c("Accuracy", "Sensitivity (Recall)", "Specificity", "Precision", "F1 Score"), each = 4),
  Model = rep(c("SVM", "LASSO", "Ridge", "LDA"), times = 5),
  Clean_Alternative_Fuel_Vehicle_Eligible = c(
    0.9985, 0.9999, 0.9706, 0.8299,  # Accuracy
    1.0000, 1.0000, 0.9317, 0.5842,  # Sensitivity
    0.9975, 1.0000, 0.9977, 1.0000,  # Specificity
    0.9963, 1.0000, 0.9964, 1.0000,  # Precision
    0.9981, 1.0000, 0.9631, 0.7376   # F1 Score
  ),
  Eligibility_Unknown = c(
    0.9985, 0.9999, 0.9706, 0.8299,  # Accuracy
    1.0000, 1.0000, 1.0000, 1.0000,  # Sensitivity
    1.0000, 0.9998, 0.9481, 0.8782,  # Specificity
    1.0000, 0.9997, 0.9455, 0.8809,  # Precision
    1.0000, 0.9999, 0.9717, 0.9265   # F1 Score
  ),
  Not_Eligible_Due_to_Low_Battery_Range = c(
    0.9985, 0.9999, 0.9706, 0.8299,  # Accuracy
    0.9872, 0.9989, 0.9872, 0.9989,  # Sensitivity
    1.0000, 1.0000, 0.9992, 0.8799,  # Specificity
    1.0000, 1.0000, 0.9936, 0.5252,  # Precision
    0.9936, 0.9994, 0.9904, 0.6883   # F1 Score
  )
)

#let's print the summary table 
print(summary_table)


#SVM and LASSO models show very high accuracy, precision, and sensitivity with LASSO slightly better than SVM
#Ridge model has a lower accuracy and F1 score compared to SVM and LASSO.
#LDA model has much lower performance in terms of accuracy, sensitivity for the "CAFV" class, and precision for the case of "Not eligible due to low battery range" class.




### Plotting Make and Model with CAFV Eligibility ###
# Make and CAFV Eligibility
ggplot(ev.data, aes(x = Make, fill = CAFV)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "CAFV Eligibility by Make",
       x = "Make",
       y = "Count")

# Model and CAFV Eligibility (Top 20 models) 
top_models <- ev.data %>% group_by(Model) %>% 
  summarize(Count = n()) %>% 
  top_n(20, Count) %>% 
  pull(Model)

ggplot(ev.data %>% filter(Model %in% top_models), aes(x=Model, fill=CAFV)) + 
  geom_bar(position="dodge") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "CAFV Eligibility for Top 20 Model", x = "Model", y = "Count")

###Analyzing CAFV Eligibility by County and City###
# County by CAFV Eligibility
ggplot(ev.data, aes(x=County, fill=CAFV)) + 
  geom_bar(position="dodge") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "County by CAFV Eligibility", x = "County", y = "Count")

# City by CAFV Eligibility (For top 20 cities)
top_cities <- ev.data %>% group_by(City) %>% 
  summarize(Count = n()) %>% 
  top_n(20, Count) %>% 
  pull(City)

ggplot(ev.data %>% filter(City %in% top_cities), aes(x=City, fill=CAFV)) + 
  geom_bar(position="dodge") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "City by CAFV Eligibility", x = "City", y = "Count")


#Box plot of Electric Range by CAFV Eligibility
ggplot(ev.data, aes(x = Clean.Alternative.Fuel.Vehicle..CAFV..Eligibility,
                    y = Electric.Range, 
                    fill = Clean.Alternative.Fuel.Vehicle..CAFV..Eligibility)) +
  geom_boxplot() +
  labs(title = "CAFV Eligibility by Electric Range",
       x = "CAFV Eligibility",
       y = "Electric Range (MPG)") +
  theme_minimal() +
  theme(legend.position = "none")



# CAFV Eligibility by Model Year
ggplot(ev.data, aes(x = Model.Year, fill = CAFV)) +
  geom_bar() +
  labs(title = "CAFV Eligibility by Model Year",
       x = "Model Year",
       y = "Count") +
  theme_minimal()

#Electric Vehicle Type by CAFV Eligibility
ggplot(ev.data, aes(x = Electric.Vehicle.Type, fill = CAFV)) +
  geom_bar(position = "dodge") +
  labs(title = "Electric Vehicle Type by CAFV Eligibility",
       x = "Electric Vehicle Type",
       y = "Count") +
  theme_minimal()





# Summary Statistics
summary_stats <- ev.data %>%
  group_by(CAFV) %>%
  summarise(
    Mean_Electric_Range = mean(Electric.Range, na.rm = TRUE),
    Mean_Base_MSRP = mean(Base.MSRP, na.rm = TRUE)
  )

# Base MSRP
ggplot(ev.data, aes(x = CAFV, y = Base.MSRP)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Base MSRP by CAFV Eligibility", x = "CAFV Eligibility", y = "Base MSRP")


# Correlation Analysis
cor_analysis <- cor(ev.data$Electric.Range, ev.data$Base.MSRP, use = "complete.obs")
# Positive correlation =  0.106064

#CAFV Eligibility across Top 10 Legislative Districts
ev.data %>%
  group_by(Legislative.District) %>%
  summarise(total_count = n(), .groups = 'drop') %>%
  top_n(10, total_count) %>%
  inner_join(ev.data, by = "Legislative.District") %>%
  group_by(Legislative.District, CAFV) %>%
  summarise(count = n(), .groups = 'drop') %>%
  ggplot(aes(x = Legislative.District, y = count, fill = CAFV)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "CAFV Eligibility across Top 10 Legislative Districts",
       x = "Legislative District",
       y = "Count",
       fill = "CAFV Eligibility") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Heatmap for CAFV Eligibility across Top 10 Counties
ev.data %>%
  group_by(County) %>%
  summarise(total_count = n()) %>%
  top_n(10, total_count) %>%
  inner_join(ev.data, by = "County") %>%
  group_by(County, CAFV) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = County, y = CAFV, fill = count)) +
  geom_tile() +
  labs(title = "CAFV Eligibility across Top 10 Counties", x = "County", y = "CAFV Eligibility") +
  scale_fill_gradient(low = "blue", high = "red")



# New Variable to introduce:

#Vehicle Age
ev.data$VehicleAge <- as.numeric(format(Sys.Date(), "%Y")) - ev.data$Model.Year

# Relationship between CAFV Eligibility and Vehicle Age
library(ggplot2)

ggplot(ev.data, aes(x = CAFV, y = VehicleAge, fill = CAFV)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  labs(title = "CAFV Eligibility vs Vehicle Age", x = "CAFV Eligibility", y = "Vehicle Age") +
  theme(legend.position = "none")

 