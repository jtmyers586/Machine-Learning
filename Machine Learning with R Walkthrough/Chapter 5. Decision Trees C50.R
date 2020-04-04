# Artificial Intelligence & Machine Learning Notebook
# from Machine Learning with R - Brent Lantz

## Understanding Decision Trees ----
# Entropy calculation: Information loss
-0.60 * log2(0.60) - 0.40 * log2(0.40)
curve(-x * log2(x) - (1 - x) * log2(1 - x),
      col = "red", xlab = "x", ylab = "Entropy", lwd = 4)

# Exploring and preparing the data ----
credit <- read.csv("C:/Users/jomyers/Desktop/Machine Learning with R Walkthrough/credit.csv")
str(credit)
table(credit$checking_balance)
table(credit$savings_balance)
summary(credit$months_loan_duration)
summary(credit$amount)
table(credit$default)

set.seed(123)
train_sample <- sample(1000, 900)
str(train_sample)

credit_train <- credit[train_sample, ]
credit_test  <- credit[-train_sample, ]

# check the proportion of class variable
prop.table(table(credit_train$default))
prop.table(table(credit_test$default))

## Step 3: Training a model on the data ----
library(C50)
credit_model <- C5.0(credit_train[-17], credit_train$default)
credit_model
summary(credit_model)

## Step 4: Evaluating model performance ----
credit_pred <- predict(credit_model, credit_test)
library(gmodels)
CrossTable(credit_test$default, credit_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

## Step 5: Improving model performance ----
credit_boost10 <- C5.0(credit_train[-17], credit_train$default,
                       trials = 10)
credit_boost10
summary(credit_boost10)
credit_boost_pred10 <- predict(credit_boost10, credit_test)
CrossTable(credit_test$default, credit_boost_pred10,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))


matrix_dimensions <- list(c("no", "yes"), c("no", "yes"))
names(matrix_dimensions) <- c("predicted", "actual")
matrix_dimensions
# cost matrix
error_cost <- matrix(c(0, 1, 4, 0), nrow = 2, dimnames = matrix_dimensions)
error_cost

# apply the cost matrix to the tree
credit_cost <- C5.0(credit_train[-17], credit_train$default,
                    costs = error_cost)
credit_cost_pred <- predict(credit_cost, credit_test)

CrossTable(credit_test$default, credit_cost_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))