# Artificial Intelligence & Machine Learning Notebook
# from Machine Learning with R - Brent Lantz

# Building a KNN Model
wdbc <- read.csv("C:/Users/jomyers/Desktop/Machine Learning with R Walkthrough/wdbc.csv", stringsAsFactors = FALSE)
wdbc <-wdbc[,-1]
str(wdbc)
table(wdbc$diagnosis)

wdbc$diagnosis <- factor(wdbc$diagnosis, levels = c("B", "M"),
                       labels = c("Benign", "Malignant"))
round(prop.table(table(wdbc$diagnosis))*100, digits = 1)

summary(wdbc[c("radius_mean", "area_mean", "smoothness")])

# Normalize using z-score.
normalize <- function(x) {
return( (x-min(x)) / (max(x)-min(x)) )
}

# Build a new dataset based on the normalized values. Test to make sure function works.
normalize(wdbc$smoothness)
wdbc_n  <- as.data.frame(lapply(wdbc[2:31], normalize))
summary(wdbc_n$area_mean)

# Split the testing and training data. 
wdbc_train <- wdbc_n[1:469, ]
wdbc_test <- wdbc_n[470:569, ]
wdbc_train_labels <- wdbc[1:469, 1]
wdbc_test_labels <- wdbc[470:569, 1]

library(class)
wdbc_test_pred <- knn(train = wdbc_train, test = wdbc_test, 
               cl = wdbc_train_labels, k = 15)

library(gmodels)
CrossTable(wdbc_test_labels, wdbc_test_pred, 
         prop.chisq = FALSE)

## Step 5: Improving model performance ----

### - Now to use the z-score transformation

wdbc_z<- as.data.frame(scale(wdbc[-1]))
summary(wdbc_z$area_mean)
wdbc_train_z<-as.data.frame(wdbc_z[1:469,])
wdbc_test_z<-as.data.frame(wdbc_z[470:569,])
wdbc_train_labels_z<-wdbc[1:469,1]
wdbc_test_labels_z<-wdbc[470:569,1]

wdbc_test_z_pred<-knn(train = wdbc_train_z, test = wdbc_test_z,
                 cl = wdbc_train_labels_z, k = 21)

CrossTable(wdbc_test_labels_z, wdbc_test_z_pred,
           prop.chisq = FALSE)


## Test wth multiple values of K
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, 
                      cl = wbcd_train_labels, k=1)

CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, 
           prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, 
                      cl = wbcd_train_labels, k=5)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, 
           prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, 
                      cl = wbcd_train_labels, k=11)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, 
           prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, 
                      cl = wbcd_train_labels, k=15)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, 
           prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, 
                      cl = wbcd_train_labels, k=21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, 
           prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, 
                      cl = wbcd_train_labels, k=27)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, 
           prop.chisq=FALSE)
