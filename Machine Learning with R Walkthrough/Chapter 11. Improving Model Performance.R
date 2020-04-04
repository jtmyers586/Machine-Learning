# Artificial Intelligence & Machine Learning Notebook
# from Machine Learning with R - Brent Lantz

# Improving Model Performance
library(caret)

## Creating a simple tuned model ----
set.seed(300)
m <- train(default ~ ., data = credit, method = "C5.0")
p <- predict(m, credit)
table(p, credit$default)

head(predict(m, credit, type = "raw"))
head(predict(m, credit, type = "prob"))

ctrl <- trainControl(method = "cv", number = 10,
                     selectionFunction = "oneSE")
grid <- expand.grid(model = "tree",
                    trials = c(1, 5, 10, 15, 20, 25, 30, 35),
                    winnow = FALSE)
set.seed(300)
m <- train(default ~ ., data = credit, method = "C5.0",
           metric = "Kappa",
           trControl = ctrl,
           tuneGrid = grid)

## Bagging ----
library(ipred)
set.seed(300)
mybag <- bagging(default ~ ., data = credit, nbagg = 25)
credit_pred <- predict(mybag, credit)
table(credit_pred, credit$default)

set.seed(300)
ctrl <- trainControl(method = "cv", number = 10)
train(default ~ ., data = credit, method = "treebag",
      trControl = ctrl)

## Boosting ----
library(C50)
m_c50_bst <- C5.0(default ~ ., data = credit, trials = 100)

library(adabag)
set.seed(300)
m_adaboost <- boosting(default ~ ., data = credit)
p_adaboost <- predict(m_adaboost, credit)
head(p_adaboost$class)
p_adaboost$confusion

set.seed(300)
adaboost_cv <- boosting.cv(default ~ ., data = credit)
adaboost_cv$confusion

library(vcd)
Kappa(adaboost_cv$confusion)

## Random Forests ----
library(randomForest)
set.seed(300)
rf <- randomForest(default ~ ., data = credit)

library(vcd)
Kappa(rf$confusion[1:2,1:2])

library(caret)
ctrl <- trainControl(method = "repeatedcv",
                     number = 10, repeats = 10,
                     selectionFunction = "best",
                     savePredictions = TRUE,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)

grid_rf <- expand.grid(mtry = c(2, 4, 8, 16))

set.seed(300)
m_rf <- train(default ~ ., data = credit, method = "rf",
              metric = "ROC", trControl = ctrl,
              tuneGrid = grid_rf)

# auto-tune a boosted C5.0 decision tree
grid_c50 <- expand.grid(model = "tree",
                        trials = c(10, 25, 50, 100),
                        winnow = FALSE)

set.seed(300)
m_c50 <- train(default ~ ., data = credit, method = "C5.0",
               metric = "ROC", trControl = ctrl,
               tuneGrid = grid_c50)


library(pROC)
roc_rf <- roc(m_rf$pred$obs, m_rf$pred$yes)
roc_c50 <- roc(m_c50$pred$obs, m_c50$pred$yes)

plot(roc_rf, col = "red", legacy.axes = TRUE)
plot(roc_c50, col = "blue", add = TRUE)
