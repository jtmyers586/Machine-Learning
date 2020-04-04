# Artificial Intelligence & Machine Learning Notebook
# from Machine Learning with R - Brent Lantz

# K means clulstering
teens <- read.csv("C:/Users/jomyers/Desktop/Machine Learning with R Walkthrough/snsdata.csv")
str(teens)
table(teens$gender)
table(teens$gender, useNA = "ifany")
summary(teens$age)

teens$age <- ifelse(teens$age >= 13 & teens$age < 20,
                    teens$age, NA)
summary(teens$age)

teens$female <- ifelse(teens$gender == "F" &
                         !is.na(teens$gender), 1, 0)
teens$no_gender <- ifelse(is.na(teens$gender), 1, 0)

table(teens$gender, useNA = "ifany")
table(teens$female, useNA = "ifany")
table(teens$no_gender, useNA = "ifany")

mean(teens$age, na.rm = TRUE)

aggregate(data = teens, age ~ gradyear, mean, na.rm = TRUE)
ave_age <- ave(teens$age, teens$gradyear,
               FUN = function(x) mean(x, na.rm = TRUE))

teens$age <- ifelse(is.na(teens$age), ave_age, teens$age)
summary(teens$age)

## Step 3: Training a model on the data ----
interests <- teens[5:40]
interests_z <- as.data.frame(lapply(interests, scale))
summary(interests$basketball)
summary(interests_z$basketball)

set.seed(2345)
teen_clusters <- kmeans(interests_z, 5)

## Step 4: Evaluating model performance ----
teen_clusters$size
teen_clusters$centers

## Step 5: Improving model performance ----
# apply the cluster IDs to the original data frame
teens$cluster <- teen_clusters$cluster
teens[1:5, c("cluster", "gender", "age", "friends")]
aggregate(data = teens, age ~ cluster, mean)
aggregate(data = teens, female ~ cluster, mean)
aggregate(data = teens, friends ~ cluster, mean)
