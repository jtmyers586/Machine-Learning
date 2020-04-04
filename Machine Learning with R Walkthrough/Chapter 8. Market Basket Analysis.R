# Artificial Intelligence & Machine Learning Notebook
# from Machine Learning with R - Brent Lantz

# Market Basket Analysis

library("arules")
groceries<-read.transactions("C:/Users/jomyers/Desktop/Machine Learning with R Walkthrough/groceries.csv", sep = ",")
summary(groceries)

inspect(groceries[1:5])

itemFrequency(groceries[,1:3])
itemFrequencyPlot(groceries, support = 0.1)
itemFrequencyPlot(groceries, topN = 20)

image(groceries[1:5])
image(sample(groceries, 100))

#Training a model on the data
apriori(groceries)
groceryrules <- apriori(groceries, parameter = list(support = 
                        0.006, confidence = 0.25, minlen = 2))
summary(groceryrules)                        

inspect(groceryrules[1:3])
inspect(sort(groceryrules, by = "lift")[1:5])

berryrules <-subset(groceryrules, items %in% "berries")
inspect(berryrules)
write(groceryrules, file = "groceryrules.csv",
      sep = ",", quote = TRUE, row.names = FALSE)

groceryrules_df <- as(groceryrules, "data.frame")
str(groceryrules_df)