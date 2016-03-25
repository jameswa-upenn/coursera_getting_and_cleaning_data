##Instructions
##The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set.
##
##Review criteria
## 1.The submitted data set is tidy. 
## 2.The Github repo contains the required scripts.
## 3.GitHub contains a code book that modifies and updates the available codebooks with the data to indicate all the variables and summaries calculated, 
##   along with units, and any other relevant information.
## 4.The README that explains the analysis files is clear and understandable.
## 5.The work submitted for this project is the work of the student who submitted it.
## 
## Getting and Cleaning Data Course Project
## The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. 
## The goal is to prepare tidy data that can be used for later analysis. 
## You will be graded by your peers on a series of yes/no questions related to the project. 
## You will be required to submit: 
##     1) a tidy data set as described below, 
##     2) a link to a Github repository with your script for performing the analysis, and 
##     3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. 
##   You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.
## 
## One of the most exciting areas in all of data science right now is wearable computing - see for example this article . 
## Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. 
## The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. 
## A full description is available at the site where the data was obtained:
##   http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 
## 
## Here are the data for the project:
##   https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
## 
## You should create one R script called run_analysis.R that does the following. 
## 1.Merges the training and the test sets to create one data set.
## 2.Extracts only the measurements on the mean and standard deviation for each measurement. 
## 3.Uses descriptive activity names to name the activities in the data set
## 4.Appropriately labels the data set with descriptive variable names. 
## 5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
## 
## Good luck!
##

library(plyr)
library(dplyr)
library(reshape2)

fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
UCIHARDataset <- "./data/UCI HAR Dataset.zip"

if (!file.exists("data")) {
  dir.create("data")
}

if (!file.exists(UCIHARDataset)){
  download.file(fileURL, destfile = UCIHARDataset, mode = "wb")
  unzip(UCIHARDataset, exdir = "./data")
  dateDownloaded <- date()
}
##- 'features.txt': List of all features. (X_train & X_test)
features <- read.table("./data/UCI HAR Dataset/features.txt", stringsAsFactors = F) ##561 obs. of 2 variables
features <- rename(features, feature_no = V1, feature_name = V2) ##rename cols in features
feature_names <- features$feature_name
feature_names <- sub("^f", "frequency_", feature_names)
feature_names <- sub("^t", "time_", feature_names)
feature_names <- sub("Acc", "accelerometer_", feature_names)
feature_names <- sub("Gyro", "gyroscope_", feature_names)
feature_names <- sub("Mag", "magnitude_", feature_names)
feature_names <- sub("Gravity", "gravity_", feature_names)
feature_names <- sub("Jerk", "jerk_", feature_names)
feature_names <- sub("BodyBody", "body_", feature_names)
feature_names <- sub("Body", "body_", feature_names)
feature_names <- gsub("-", "", feature_names)
feature_names <- tolower(feature_names)

##- 'train/X_train.txt': Training set.
X_train <- read.table("./data/UCI HAR Dataset/train/X_train.txt", stringsAsFactors = F) ##dim: [1] 7352  561 (V1 - V561) / features
colnames(X_train) <- feature_names ##renaming columns with the 561 feature names

##- 'test/X_test.txt': Test set.
X_test <- read.table("./data/UCI HAR Dataset/test/X_test.txt", stringsAsFactors = F) ##dim: [1] 2947  561 (V1 - V561) / features
colnames(X_test) <- feature_names ##renaming columns with the 561 feature names

##- 'activity_labels.txt': Links the class labels with their activity name: (Y-train & Y_test)
activity_labels <- read.table("./data/UCI HAR Dataset/activity_labels.txt", stringsAsFactors = F) ## 6 obs. of 2 variable
activity_labels <- rename(activity_labels, activity_no = V1, activity_type = V2) ##rename cols in activity_labels

##- 'train/y_train.txt': Training labels.
Y_train <- read.table("./data/UCI HAR Dataset/train/Y_train.txt", stringsAsFactors = F) ##dim: [1] 7352    1 / activities
Y_train <- rename(Y_train, activity_no = V1)
Y_train <- join(Y_train, activity_labels, by= "activity_no") ##adding column containing activity lookup value

##- 'test/y_test.txt': Test labels.
Y_test <- read.table("./data/UCI HAR Dataset/test/Y_test.txt", stringsAsFactors = F) ##dim: [1] 2947    1 / activities
Y_test <- rename(Y_test, activity_no = V1)
Y_test <- join(Y_test, activity_labels, by= "activity_no")  ##adding column containing activity lookup value

##- 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 
subject_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt", stringsAsFactors = F) ##7,352 obs. of 1 variable 
subject_train <- rename(subject_train, subject_id = V1) ##renaming column as subject_id (30 subjects)

##? no mention in dataset sheet
subject_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt", stringsAsFactors = F) ##2,947 obs. of 1 variable 
subject_test <- rename(subject_test, subject_id = V1) ##renaming column as subject_id (30 subjects)

cbind_train <- cbind(X_train, Y_train, subject_train) #combining the three 'train' tables

cbind_test <- cbind(X_test, Y_test, subject_test) #combining the three 'test' tables

rbind_dataset <- rbind(cbind_train, cbind_test) #combining the 'train'/'test' tables

colgr <- grep("mean\\()|std\\()", names(rbind_dataset), value = TRUE) #dataset column names including "mean()" and/or "std()" but excluding "meanFreq()"

subset_data <- rbind_dataset[ , c('subject_id', 'activity_type', colgr)] #subset dataset to house only the columns required

subset_data_melt <- melt(subset_data, id = c('subject_id', 'activity_type')) 

subset_data_mean <- dcast(subset_data_melt, subject_id + activity_type ~ variable, mean)

write.table(subset_data_mean, file = "TidyDataSet.txt", row.names = FALSE) 
