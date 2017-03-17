rm(list = ls())
setwd( "/Users/haicen/Desktop/UCI HAR Dataset")
library(dplyr)
## train set
x.train <- read.table("/Users/haicen/Desktop/UCI HAR Dataset/train/X_train.txt", 
                 header = FALSE)
y.train <- read.table("/Users/haicen/Desktop/UCI HAR Dataset/train/y_train.txt", 
                      header = FALSE)
features <- read.table("/Users/haicen/Desktop/UCI HAR Dataset/features.txt", 
                       header = FALSE)
subjectID_train<- read.table("/Users/haicen/Desktop/UCI HAR Dataset/train/subject_train.txt", 
                             header = FALSE)

## View the data
View(y.train)
dim(y.train)
View(x.train)
dim(x.train)
View(features)
# get the feature names for each columns
FeatureNames <- features[,2] 
# get the subject ID
subjectID_train<- read.table("/Users/haicen/Desktop/UCI HAR Dataset/train/subject_train.txt", 
header = FALSE)
names(subjectID_train) <- "SubjectID"
names(x.train) <- FeatureNames
names(y.train) <- "Activity.Labels"
Train_Original <- cbind(subjectID_train, y.train, x.train)
###
# test set
x.test <- read.table("/Users/haicen/Desktop/UCI HAR Dataset/test/X_test.txt", 
                      header = FALSE)
y.test <- read.table("/Users/haicen/Desktop/UCI HAR Dataset/test/y_test.txt", 
                      header = FALSE)
subjectID_test<- read.table("/Users/haicen/Desktop/UCI HAR Dataset/test/subject_test.txt", 
                             header = FALSE)
names(subjectID_test) <- "SubjectID"
names(x.test) <- FeatureNames
names(y.test) <- "Activity.Labels"
Test_Original <- cbind(subjectID_test, y.test, x.test)

# merge the traning set and testing data set
FullData <- rbind(Train_Original, Test_Original)

# Extracts only the measurements on the mean
## and standard deviation for each measurement.
selectMean <- as.vector(grep('mean', FeatureNames, value = T))
selectStd <- as.vector(grep('std', FeatureNames, value = T))
selectNames <- c(selectMean, selectStd)

FinalData <- FullData[, c("SubjectID", "Activity.Labels",selectMean, selectStd)]
## order the dataset by the subject names and Activity.Labels
FinalData <- arrange(FinalData, SubjectID, Activity.Labels)
## Uses descriptive activity names to name the activities in the data set
activity <- read.table("/Users/haicen/Desktop/UCI HAR Dataset/activity_labels.txt", 
                              header = FALSE)
activity_labels <- activity[ , 2]
activity_labels <- as.vector(activity_labels)

for (i in 1:6){
    FinalData[FinalData[, 2] == i, 2] <- activity_labels[i]
}

# save the dataset and give it a name
write.csv(FinalData, file ='HumanActivityRecognitionUsingSmartphones.csv')
FinalData <- read.csv('HumanActivityRecognitionUsingSmartphones.csv', header = T)
##From the data set in step 4, creates a second, 
##independent tidy data set with the average 
##of each variable for each activity and each subject.
testdd <- FinalData[,-1]
# get average of each meature grouped by subjects and activities.
ag <- aggregate(testdd, by = list(testdd$SubjectID, testdd$Activity.Labels),  mean)
ag1 <- ag[, -(3:4)]
# rename the columns 
names(ag1) <-c("SubjectID", 'Activity.Labels', paste("Mean_", names(ag1)[-(1:2)]))
# save the data in a new txt file
write.table(ag1, file = 'AverageOfVariablesForSubjectAndActicity.txt', row.names = F)

