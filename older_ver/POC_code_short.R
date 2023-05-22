library(tidyverse)
library(readxl)

# loading data
data <- read_excel("D:/07_r_programmering_med_tillämpningar_inom_dataanalys/kunskapskontroll/del 1/data_collection.xlsx", sheet = "raw_data")

# head of data
head(data)

# feature names
names(data)

# data structure
str(data)

# summary of data
summary(data)

# unique values for variables
unique(data$Biltyp)
unique(data$Växellåda)
unique(data$Bränsle)
unique(data$Miltal)

# away with the hybrid!
data <- data[data$Bränsle!= "Miljöbränsle/Hybrid", ]

# Miltal category to the middle value of the category
data <- data %>%
  separate(Miltal, into = c("Lägsta_miltal", "Högsta_miltal"), sep = " - ") %>%
  mutate(
    Lägsta_miltal = str_replace_all(Lägsta_miltal, " ", ""),
    Högsta_miltal = str_replace_all(Högsta_miltal, " ", ""),
    Miltal_mitten = round((as.numeric(Lägsta_miltal) + as.numeric(Högsta_miltal)) / 2)
  )%>%
  select(-Lägsta_miltal, -Högsta_miltal)

# any missing values?
sum(is.na(data))

# take them away
data <- na.omit(data)

# any missing values now?
sum(is.na(data))

# the structure after preprocessing
str(data)

# columns after preprocessing
names(data)

unique(data$Biltyp)
unique(data$Växellåda)
unique(data$Bränsle)
unique(data$Miltal_mitten)


# Data splitting (Ranom sample)----------------------------------------------------------

set.seed(42)

X_train_index <- sample(1:nrow(data), nrow(data)*0.8)
X_train <- data[X_train_index, ]
X_test <- data[-X_train_index, ]
y_test <- data[-X_train_index, 5]


# cv on train data --------------------------------------------------------
set.seed(42) 
library(boot)
mdl1=glm(Pris~., data=data, subset = X_train_index)
cv.err=cv.glm(X_train,mdl1, K=10)
cv.err$delta

cv_train_rmse <- sqrt(cv.err$delta[2])
cv_train_rmse


# Prediction on training data (just in case) ---------------------------------------------

names(data)
set.seed(42) 

y_train <- X_train[, 5]
# y_train

predict_train <- predict(mdl1, newdata = X_train, type = "response")
# predict_train

residuals_train <- predict_train - y_train
# residuals_train

squared_error_train <- residuals_train^2

mse_train <- sum(squared_error_train)/nrow(squared_error_train)

rmse_train <- sqrt(mse_train)
rmse_train




# Stratified train/test split (based on Model year)---------------------------------------------

library(caret)

set.seed(42) # För reproducerbarhet
X_train_strat_index <- createDataPartition(data$Modellår, p = 0.8, list = FALSE)
X_train_strat <- data[X_train_strat_index, ]
X_test_strat <- data[-X_train_strat_index, ]
y_test_strat <- X_test_strat$Pris


# CV on training data ---------------------------------------------
set.seed(42) 
library(boot)
mdl2 <- glm(Pris ~ ., data = data, subset = X_train_strat_index)
set.seed(42)
cv.err2 <- cv.glm(X_train_strat, mdl2, K = 10)
cv.err2$delta[2]

cv_rmse_strat_train <- sqrt(cv.err2$delta[2])
cv_rmse_strat_train


# Prediction on training data (just in case)---------------------------------------------
set.seed(42) 

y_train_strat <- X_train_strat[, 5]

predict_strat_train <- predict(mdl2, newdata = X_train_strat, type = "response")
# predict_train

residuals_strat_train <- predict_strat_train - y_train_strat
# residuals_train

squared_error_strat_train <- residuals_strat_train^2

mse_strat_train <- sum(squared_error_strat_train)/nrow(squared_error_strat_train)

rmse_strat_train <- sqrt(mse_strat_train)
rmse_strat_train


# Results -----------------------------------------------------------------

print('Random split CV RMSE score:')
print(cv_train_rmse)
print('Stratified split CV RMSE score:')
print(cv_rmse_strat_train)

print('Stratified split gave a little better result, so we will evaluate the model on the stratified test set')

# Prediction on test data -------------------------------------------------
set.seed(42) 

predict_strat_test <- predict(mdl2, newdata = X_test_strat, type = "response")
# predict_strat_test

residuals_strat_test <- predict_strat_test - y_test_strat
# residuals_strat_test

squared_error_strat_test <- residuals_strat_test^2

mse_strat_test <- sum(squared_error_strat_test)/length(squared_error_strat_test)

rmse_strat_test <- sqrt(mse_strat_test)
rmse_strat_test
