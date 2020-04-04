# Artificial Intelligence & Machine Learning Notebook
# from Machine Learning with R - Brent Lantz

# Black Box Methods, NN and SVM
# Part 1: Neural Networks ------------------------------

concrete<-read.csv("C:/Users/jomyers/Desktop/Machine Learning with R Walkthrough/concrete.csv", stringsAsFactors = FALSE)
summary(concrete)
str(concrete)

#normalize values
normalize <-function(x){
  return( (x-min(x) / max(x)-min(x)) )
}

concrete_norm <- as.data.frame(lapply(concrete, normalize))

summary(concrete_norm$strength)

concrete_train <- concrete_norm[1:773,]
concrete_test <- concrete_norm[774:1030,]

library("neuralnet")
set.seed(12345)
concrete_model <- neuralnet(strength ~ cement + slag + ash + water + superplastic + coarseagg + fineagg + age,
                            data = concrete)
plot(concrete_model)
model_results <- compute(concrete_model, concrete_test[1:8])
predicted_strength <- model_results$net.result
cor(predicted_strength, concrete_test$strength)

#Improving model performance
concrete_model2 <- neuralnet(strength ~ cement + slag +
                               ash + water + superplastic +
                               coarseagg + fineagg + age,
                             data = concrete_train, hidden = 5)

plot(concrete_model2)

model_results2 <- compute(concrete_model2, concrete_test[1:8])
predicted_strength2<-model_results$net.result
cor(predicted_strength2, concrete_test$strength)

softplus<-function(x){log(1+exp(x))}
set.seed(12345)
concrete_model3 <- neuralnet(strength ~ cement + slag + 
                               ash + watere + superplastic + 
                               courseagg + fineagg + age,
                             data = concrete_train,
                             hidden = c(5,5), 
                             act.fct = softplus)
plot(concrete_model3)

model_results3<-compute(concrete_model3, concrete_test[1:8])
predicted_strength3<-model_results$net.result
cor(predicted_strength3, concrete_test$strength)

stengths<-data.frame(
  actual = concrete$strength[774:1030],
  pred = predicted_strength3
)

head(strengths, n = 3)

cor(strengths$pred, strengths$actual)

unnormalize <- function(x){
  return(x * (max(concrete$strength) - 
                 min(concrete$strength) + min(concrete$strength)))
}

strengths$pred_new <- unnormalize(strengths$pred)
strengths$error <- strengths$pred_new - strengths$actual

head(strengths, n  = 3)

cor(strengths$pred_new, strengths$actual)


##### Part 2: Support Vector Machines -------------------
letters <- read.csv("C:/Users/jomyers/Desktop/Machine Learning with R Walkthrough/letterdata.csv")
str(letters)
letters_train <- letters[1:16000, ]
letters_test  <- letters[16001:20000, ]

## Step 3: Training a model on the data ----
library(kernlab)
letter_classifier <- ksvm(letter ~ ., data = letters_train,
                          kernel = "vanilladot")
letter_classifier

## Step 4: Evaluating model performance ----
letter_predictions <- predict(letter_classifier, letters_test)
head(letter_predictions)
table(letter_predictions, letters_test$letter)

agreement <- letter_predictions == letters_test$letter
table(agreement)
prop.table(table(agreement))

## Step 5: Improving model performance ----
set.seed(12345)
letter_classifier_rbf <- ksvm(letter ~ ., data = letters_train, kernel = "rbfdot")
letter_predictions_rbf <- predict(letter_classifier_rbf, letters_test)
agreement_rbf <- letter_predictions_rbf == letters_test$letter
table(agreement_rbf)
prop.table(table(agreement_rbf))

cost_values <- c(1, seq(from = 5, to = 40, by = 5))
accuracy_values <- sapply(cost_values, function(x) {
  set.seed(12345)
  m <- ksvm(letter ~ ., data = letters_train,
            kernel = "rbfdot", C = x)
  pred <- predict(m, letters_test)
  agree <- ifelse(pred == letters_test$letter, 1, 0)
  accuracy <- sum(agree) / nrow(letters_test)
  return (accuracy)
})

plot(cost_values, accuracy_values, type = "b")