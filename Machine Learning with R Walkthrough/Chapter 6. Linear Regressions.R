# Artificial Intelligence & Machine Learning Notebook
# from Machine Learning with R - Brent Lantz

# Linear Regression Analysis
launch <- read.csv("C:/Users/jomyers/Desktop/Machine Learning with R Walkthrough/challenger.csv")
b <- cov(launch$temperature, launch$distress_ct) / var(launch$temperature)
a <- mean(launch$distress_ct) - b * mean(launch$temperature)
r <- cov(launch$temperature, launch$distress_ct) /
       (sd(launch$temperature) * sd(launch$distress_ct))
cor(launch$temperature, launch$distress_ct)

r * (sd(launch$distress_ct) / sd(launch$temperature))
model <- lm(distress_ct ~ temperature, data = launch)
summary(model)

# creating a simple multiple regression function
reg <- function(y, x) {
  x <- as.matrix(x)
  x <- cbind(Intercept = 1, x)
  b <- solve(t(x) %*% x) %*% t(x) %*% y
  colnames(b) <- "estimate"
  print(b)
}

# examine the launch data
str(launch)
reg(y = launch$distress_ct, x = launch[2])
reg(y = launch$distress_ct, x = launch[2:4])
model <- lm(distress_ct ~ temperature + field_check_pressure + flight_num, data = launch)
model

## Example: Predicting Medical Expenses ----
## Step 2: Exploring and preparing the data ----
insurance <- read.csv("insurance.csv", stringsAsFactors = TRUE)
str(insurance)
summary(insurance$expenses)
hist(insurance$expenses)
table(insurance$region)
cor(insurance[c("age", "bmi", "children", "expenses")])
pairs(insurance[c("age", "bmi", "children", "expenses")])
library(psych)
pairs.panels(insurance[c("age", "bmi", "children", "expenses")])

## Step 3: Training a model on the data ----
ins_model <- lm(expenses ~ age + children + bmi + sex + smoker + region,
                data = insurance)
ins_model

## Step 4: Evaluating model performance ----
summary(ins_model)

## Step 5: Improving model performance ----
insurance$age2 <- insurance$age^2
insurance$bmi30 <- ifelse(insurance$bmi >= 30, 1, 0)
ins_model2 <- lm(expenses ~ age + age2 + children + bmi + sex +
                   bmi30*smoker + region, data = insurance)
summary(ins_model2)
insurance$pred <- predict(ins_model2, insurance)
cor(insurance$pred, insurance$expenses)
plot(insurance$pred, insurance$expenses)
abline(a = 0, b = 1, col = "red", lwd = 3, lty = 2)
predict(ins_model2,
        data.frame(age = 30, age2 = 30^2, children = 2,
                   bmi = 30, sex = "male", bmi30 = 1,
                   smoker = "no", region = "northeast"))
predict(ins_model2,
        data.frame(age = 30, age2 = 30^2, children = 2,
                   bmi = 30, sex = "female", bmi30 = 1,
                   smoker = "no", region = "northeast"))
predict(ins_model2,
        data.frame(age = 30, age2 = 30^2, children = 0,
                   bmi = 30, sex = "female", bmi30 = 1,
                   smoker = "no", region = "northeast"))

#### Part 2: Regression Trees and Model Trees -------------------
tee <- c(1, 1, 1, 2, 2, 3, 4, 5, 5, 6, 6, 7, 7, 7, 7)
at1 <- c(1, 1, 1, 2, 2, 3, 4, 5, 5)
at2 <- c(6, 6, 7, 7, 7, 7)
bt1 <- c(1, 1, 1, 2, 2, 3, 4)
bt2 <- c(5, 5, 6, 6, 7, 7, 7, 7)

sdr_a <- sd(tee) - (length(at1) / length(tee) * sd(at1) + length(at2) / length(tee) * sd(at2))
sdr_b <- sd(tee) - (length(bt1) / length(tee) * sd(bt1) + length(bt2) / length(tee) * sd(bt2))

sdr_a
sdr_b
## Example: Estimating Wine Quality ----
## Step 2: Exploring and preparing the data ----
wine <- read.csv("C:/Users/jomyers/Desktop/Machine Learning with R Walkthrough/whitewines.csv")
str(wine)
hist(wine$quality)
summary(wine)

wine_train <- wine[1:3750, ]
wine_test <- wine[3751:4898, ]

## Step 3: Training a model on the data ----
library(rpart)
m.rpart <- rpart(quality ~ ., data = wine_train)
m.rpart
summary(m.rpart)

library(rpart.plot)
rpart.plot(m.rpart, digits = 3)
rpart.plot(m.rpart, digits = 4, fallen.leaves = TRUE, type = 3, extra = 101)

## Step 4: Evaluate model performance ----
p.rpart <- predict(m.rpart, wine_test)
summary(p.rpart)
summary(wine_test$quality)
cor(p.rpart, wine_test$quality)
MAE <- function(actual, predicted) {
  mean(abs(actual - predicted))  
}
MAE(p.rpart, wine_test$quality)

mean(wine_train$quality)
MAE(5.87, wine_test$quality)

## Step 5: Improving model performance ----
library(Cubist)
m.cubist <- cubist(x = wine_train[-12], y = wine_train$quality)
summary(m.cubist)
p.cubist <- predict(m.cubist, wine_test)
summary(p.cubist)
cor(p.cubist, wine_test$quality)
MAE(wine_test$quality, p.cubist)