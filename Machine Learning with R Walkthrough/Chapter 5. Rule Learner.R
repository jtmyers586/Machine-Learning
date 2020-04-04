# Artificial Intelligence & Machine Learning Notebook
# from Machine Learning with R - Brent Lantz

# Using the C5.0 Model for rule learners
# Greedy rules for pruning


mushrooms <- read.csv("mushrooms.csv", stringsAsFactors = TRUE)
str(mushrooms)
mushrooms$veil_type <- NULL
table(mushrooms$type)

## Step 3: Training a model on the data ----
library(OneR)
mushroom_1R <- OneR(type ~ ., data = mushrooms)

## Step 4: Evaluating model performance ----
mushroom_1R
mushroom_1R_pred <- predict(mushroom_1R, mushrooms)
table(actual = mushrooms$type, predicted = mushroom_1R_pred)

## Step 5: Improving model performance ----
library(RWeka)
mushroom_JRip <- JRip(type ~ ., data = mushrooms)
mushroom_JRip
summary(mushroom_JRip)

# Rule Learner Using C5.0 Decision Trees (not in text)
library(C50)
mushroom_c5rules <- C5.0(type ~ odor + gill_size, data = mushrooms, rules = TRUE)
summary(mushroom_c5rules)
