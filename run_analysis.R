library(data.table)
library(dplyr)
library(tidyr)

setwd("//cib.net/shareparis/Home13/silvaol/PRIVE/COURSERA/Data Science R/getdata_Dataset/UCI HAR Dataset")

##Step 1 - Merges the training and the test sets to create one data set

## Read the data
features <- read.table("./features.txt",header=FALSE)
activity_label <- read.table("./activity_labels.txt",header=FALSE)

x_train <- read.table("./train/X_train.txt", header=FALSE)
y_train <- read.table("./train/y_train.txt", header=FALSE)
subject_train <- read.table("./train/subject_train.txt", header=FALSE)

x_test <- read.table("./test/X_test.txt", header=FALSE)
y_test <- read.table("./test/y_test.txt", header=FALSE)
subject_test <- read.table("./test/subject_test.txt", header=FALSE)

## Label the columns
colnames(activity_label)<-c("Id_Activity","Type_Activity")
colnames(subject_train) <- "subject_Id"
colnames(x_train) <- features[,2]
colnames(y_train) <- "Id_Activity"
colnames(subject_test) <- "subject_Id"
colnames(x_test) <- features[,2]
colnames(y_test) <- "Id_Activity"

## merge the tables
Data_Train<-cbind(subject_train,x_train,y_train)
Data_Test<-cbind(subject_test,x_test,y_test)
Data_global<-rbind(Data_Train,Data_Test)

## Step 2 - Extract only the measurements on the mean and standard deviation
mean_std <- (grepl("-(mean|std)\\(\\)", features[, 2])| grepl("Id_Activity", features[, 2]))
Data_mean_std <- Data_global[, mean_std]

## Step 3 Uses descriptive activity names to name the activities in the data set

Data_mean_std_Label <- merge(Data_mean_std, activity_label,by='Id_Activity', all.x=TRUE)

## Step 4 Appropriately labels the data set with descriptive variable names

names(Data_mean_std_Label) <- gsub("^f","FrequencyDomain_",names(Data_mean_std_Label))
names(Data_mean_std_Label) <- gsub("^t","TimeDomain_",names(Data_mean_std_Label))
names(Data_mean_std_Label) <- gsub("Acc","Acceleration_",names(Data_mean_std_Label))
names(Data_mean_std_Label) <- gsub("Gyro", "Gyroscope_",names(Data_mean_std_Label))
names(Data_mean_std_Label) <- gsub("Mag", "Magnitude_",names(Data_mean_std_Label))
names(Data_mean_std_Label) <- gsub("[\\(\\)-]", "",names(Data_mean_std_Label))
names(Data_mean_std_Label) <- gsub("mean", "Mean_",names(Data_mean_std_Label))
names(Data_mean_std_Label) <- gsub("std", "StandardDeviation_",names(Data_mean_std_Label))

## Step 5 From the data set in step 4, creates a second, independent tidy data set 
## with the average of each variable for each activity and each subject

Average_Global <- aggregate(. ~subject_Id + Id_Activity, Data_mean_std_Label, mean)

write.table(Average_Global, "tidy_data.txt", row.name=FALSE)
