library(tidyverse)
# library(tidyr)
# library(dplyr)
library(readxl)

# loading data
data <- read_excel("D:/07_r_programmering_med_tillämpningar_inom_dataanalys/kunskapskontroll/del 1/data_collection.xlsx", sheet = "raw_data")

# head of data
head(data)

# feature names
names(data)

# data structure
str(data)

# unique values for variables
unique(data$Biltyp)
unique(data$Växellåda)
unique(data$Bränsle)
unique(data$Miltal)

# summary of data
summary(data)

# only to numeric!!!
pairs(data)
cor(data)

# away with the hybrid!
data <- data[data$Bränsle!= "Miljöbränsle/Hybrid", ]

data <- data %>%
  separate(Miltal, into = c("Lägsta_miltal", "Högsta_miltal"), sep = " - ") %>%
  mutate(
    Lägsta_miltal = str_replace_all(Lägsta_miltal, " ", ""),
    Högsta_miltal = str_replace_all(Högsta_miltal, " ", ""),
    Miltal_mitten = round((as.numeric(Lägsta_miltal) + as.numeric(Högsta_miltal)) / 2)
  )%>%
  select(-Lägsta_miltal, -Högsta_miltal)
data

sum(is.na(data))

data <- na.omit(data)

str(data)


unique(data$Biltyp)
unique(data$Växellåda)
unique(data$Bränsle)
unique(data$Miltal_mitten)


##################################################################
set.seed(42)

X_train <- sample(1:nrow(data), nrow(data)*0.8)
X_test <- data[-X_train, ]

y_test <- data[-X_train, 5]


mdl1 <- lm(Pris~., data=data, subset = X_train)

summary(mdl1)


# cv on train data --------------------------------------------------------


library(boot)
cv.mdl1.train=glm(Pris~., data=data, subset = X_train)
cv.err=cv.glm(data[X_train, ],cv.mdl1.train)
cv.err$delta

sqrt(sum(cv.err$delta[2])/length(X_train))



?cv.glm


library(boot) # Skapa en linjär regression modell 
model <- lm(Pris~., data=data, subset = X_train) 
# Definiera en funktion som beräknar RMSE 
rmse <- function(y, yhat) { sqrt(mean((y - yhat)^2)) } 
# Beräkna RMSE med 10-faldig korsvalidering 
cv <- cv.glm(data[X_train, ],cv.mdl1.train, K = 10, loss = function(y, yhat) rmse(y, yhat)) 
# Visa RMSE för varje testuppsättning 
cv$delta 
# Beräkna genomsnittlig RMSE 
mean(cv$delta)

