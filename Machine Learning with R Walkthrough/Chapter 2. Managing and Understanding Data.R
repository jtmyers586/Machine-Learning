# Artificial Intelligence & Machine Learning Notebook
# from Machine Learning with R - Brent Lantz

# R data structures

# create vectors of data for three medical patients
subject_name <- c("John Doe", "Jane Doe", "Steve Graves")
temperature <- c(98.1, 98.6, 101.4)
flu_status <- c(FALSE, FALSE, TRUE)

temperature[2]
temperature[2:3]
temperature[-2]
temperature[c(TRUE, TRUE, FALSE)]

## Factors -----
gender <- factor(c("MALE", "FEMALE", "MALE"))
blood <- factor(c("O", "AB", "A"),
                levels = c("A", "B", "AB", "O"))
symptoms <- factor(c("SEVERE", "MILD", "MODERATE"),
                   levels = c("MILD", "MODERATE", "SEVERE"),
                   ordered = TRUE)
symptoms > "MODERATE"

## Lists -----
subject_name[1]
temperature[1]
flu_status[1]
gender[1]
blood[1]
symptoms[1]

subject1 <- list(fullname = subject_name[1], 
                 temperature = temperature[1],
                 flu_status = flu_status[1],
                 gender = gender[1],
                 blood = blood[1],
                 symptoms = symptoms[1])

subject1
subject1[2]
subject1[[2]]
subject1$temperature
subject1[c("temperature", "flu_status")]
subject1[2:3]

## Data frames -----
pt_data <- data.frame(subject_name, temperature, flu_status, gender,
                      blood, symptoms, stringsAsFactors = FALSE)
pt_data
pt_data$subject_name
pt_data[c("temperature", "flu_status")]
pt_data[2:3]
pt_data[1, 2]
pt_data[c(1, 3), c(2, 4)]

pt_data[c(1, 3), c("temperature", "gender")]
pt_data[-2, c(-1, -3, -5, -6)]
pt_data$temp_c <- (pt_data$temperature - 32) * (5 / 9)
pt_data[c("temperature", "temp_c")]

## Matrixes -----
m <- matrix(c(1, 2, 3, 4), nrow = 2)
m <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
m <- matrix(c(1, 2, 3, 4, 5, 6), ncol = 2)

##### Managing data with R ------------

## saving, loading, and removing R data structures

# show all data structures in memory
ls()
rm(m, subject1)
ls()
rm(list=ls())

##### Exploring and understanding data --------------------
usedcars <- read.csv("C:/Users/jomyers/Desktop/Machine Learning with R Walkthrough/usedcars.csv", stringsAsFactors = FALSE)
str(usedcars)

## Exploring numeric variables -----
summary(usedcars$year)
summary(usedcars[c("price", "mileage")])
range(usedcars$price)
diff(range(usedcars$price))
IQR(usedcars$price)

quantile(usedcars$price)
quantile(usedcars$price, probs = c(0.01, 0.99))
quantile(usedcars$price, seq(from = 0, to = 1, by = 0.20))

boxplot(usedcars$price, main="Boxplot of Used Car Prices",
      ylab="Price ($)")
boxplot(usedcars$mileage, main="Boxplot of Used Car Mileage",
      ylab="Odometer (mi.)")

hist(usedcars$price, main = "Histogram of Used Car Prices",
     xlab = "Price ($)")
hist(usedcars$mileage, main = "Histogram of Used Car Mileage",
     xlab = "Odometer (mi.)")


var(usedcars$price)
sd(usedcars$price)
var(usedcars$mileage)
sd(usedcars$mileage)

## Exploring numeric variables -----

table(usedcars$year)
table(usedcars$model)
table(usedcars$color)

model_table <- table(usedcars$model)
prop.table(model_table)

color_table <- table(usedcars$color)
color_pct <- prop.table(color_table) * 100
round(color_pct, digits = 1)

## Exploring relationships between variables -----
plot(x = usedcars$mileage, y = usedcars$price,
     main = "Scatterplot of Price vs. Mileage",
     xlab = "Used Car Odometer (mi.)",
     ylab = "Used Car Price ($)")

usedcars$conservative <-
  usedcars$color %in% c("Black", "Gray", "Silver", "White")

table(usedcars$conservative)

# Crosstab of conservative by model
library(gmodels)
CrossTable(x = usedcars$model, y = usedcars$conservative)
