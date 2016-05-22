#setwd('C:\\Users\\Alex\\Desktop\\R_Coursera\\data-cleaning\\w4\\w4project\\UCI HAR Dataset')
library(dplyr)
library(data.table)

## Load Activity Labels into 2 columns: number and activity description (text)
activities <- read.table('activity_labels.txt')
names(activities) <- c('ActivityNumber','Activity') ## column names

## Load Features into 2 columns: number and feature description (text)
features <- read.table('features.txt')
names(features) <- c('FeatureNumber','FeatureName') ## column names
features$FeatureName <- as.character(features$FeatureName) ## turn the feature names into character strings

## Get indices of mean() and std() features
index.mean <- grep("-mean\\(\\)",features$FeatureName)
index.std <- grep("-std\\(\\)",features$FeatureName)
index.all <- sort(c(index.mean,index.std)) ## Sorted list of indices for mean and std
features <- features[index.all,] ## select only those features of "mean" or "std"

## TEST DATA
## Load Test Data, extracting only columns index.cols (mean() and std())
test.data <- read.table('./test/X_test.txt')[,features$FeatureNumber]
names(test.data) <- features$FeatureName ## column names

## Load Test activity info and add to test.data using 'activities'
test.activity <- read.table('./test/y_test.txt')
test.data$ActivityNumber <- test.activity[,]
## match activity number with the activity description
test.data$Activity <- sapply(test.activity,function(x) x <- activities$Activity[x])

## Load and add Subject Numbers to test.data
subjects.test <- read.table('./test/subject_test.txt')
test.data$SubjectNumber <- subjects.test[,]
test.data$GroupType <- rep('TEST',nrow(test.data)) ## add a group type as a form of controlling the data

## TRAIN DATA
## Load Training Data, extracting only columns index.cols (mean() and std())
train.data <- read.table('./train/X_train.txt')[,features$FeatureNumber]
names(train.data) <- features$FeatureName ## column names

## Load Train activity info and add to test.data using 'activities'
train.activity <- read.table('./train/y_train.txt')
train.data$ActivityNumber <- train.activity[,]
## match activity number with the activity description
train.data$Activity <- sapply(train.activity,function(x) x <- activities$Activity[x])

## Load and add Subject Numbers to test.data
subjects.train <- read.table('./train/subject_train.txt')
train.data$SubjectNumber <- subjects.train[,]
train.data$GroupType <- rep('TRAIN',nrow(train.data)) ## add a group type as a form of controlling the data

## Merge the two datasets TEST and TRAIN into one dataset
all.data <- data.table(rbind(test.data,train.data[,]))

## Create a 2nd clean dataset of average values for each subject and activity
## Use 'aggregate' to sort the data into groups by (i) Subject Number and (ii) Activity.
## Take the mean of each of these groups
avg.data <- aggregate(all.data, list(all.data$SubjectNumber,all.data$ActivityNumber), mean)

## Clean avg.data by removing unused columns
avg.data$Activity <- sapply(avg.data$Group.2,function(x) activities$Activity[x]) ## Activity (descriptive text) needs to be recalculated
avg.data <- avg.data[,-c(1,2,69,72)] ## Remove unusedd/duplicate columns
avg.data <- avg.data[,c(68,67,1:66)] ## Reorder the dataset so the subject number and activity type occupy the first two columns

## Write the dataset out as a table (comment out if not necessary)
write.table(avg.data, 'tidydata_averages.txt', row.names=FALSE)