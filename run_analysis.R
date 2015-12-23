# Getting and Cleaning Data - Project
# This script will use the UCI HAR Dataset 

# Clean up workspace
rm(list=ls())

# 1. Merge the training and the test sets to create one data set.

#set working directory
 setwd('/Users/Diane/Documents/CDS_Data/');

#Download the file, remove the variable holding the URL data and unzip files
 zipfile <- "getdata_dataset.zip"
 if (!file.exists(zipfile)){
   fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
   download.file(fileURL, zipfile)
   rm(fileURL)
 }
 
 if(!file.exists("UCI HAR Dataset")){
   unzip(zipfile)
 }

 #reset working directory
 setwd('/Users/Diane/Documents/CDS_Data/UCI HAR Dataset');
 
# Read and import the data from files in the UCI HAR Dataset folder
features     = read.table('./features.txt',header=FALSE);
activityType = read.table('./activity_labels.txt',header=FALSE);

# Read and import the data files in the train sub-folder
subjectTrain = read.table('./train/subject_train.txt',header=FALSE);
xTrain       = read.table('./train/x_train.txt',header=FALSE); 
yTrain       = read.table('./train/y_train.txt',header=FALSE); 

# Assigin column names to the train data imported above
colnames(activityType)  = c('activityId','activityType');
colnames(subjectTrain)  = "subjectId";
colnames(xTrain)        = features[,2]; 
colnames(yTrain)        = "activityId";

# Create trainingData set by merging yTrain, subjectTrain, and xTrain
trainingData = cbind(yTrain,subjectTrain,xTrain);

# Read and import the data files in the test sub-folder
subjectTest = read.table('./test/subject_test.txt',header=FALSE);
xTest       = read.table('./test/x_test.txt',header=FALSE);
yTest       = read.table('./test/y_test.txt',header=FALSE);

# Assign column names to the test data imported above
colnames(subjectTest) = "subjectId";
colnames(xTest)       = features[,2]; 
colnames(yTest)       = "activityId";

# Create the testData set by merging the yTest, subjectTest ad xTest data
testData = cbind(yTest,subjectTest,xTest);

# Create the finalData set by combining trainingData and testData
finalData = rbind(trainingData,testData);

# Create a vector with the column names from the finalData,
# to be used to extract the mean() & stddev() columns
colNames  = colnames(finalData); 

# 2. Extract only the measurements on the mean and standard deviation for each measurement. 

# Create a logicalVector that contains TRUE values for the ID, mean() & stddev() columns and FALSE for others
logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));

# Subset finalData table based on the logicalVector to keep only desired columns
finalData = finalData[logicalVector==TRUE];

# 3. Use descriptive activity names to name the activities in the data set

# Merge the finalData set with the activityType table to include 
#   activity name descriptions
finalData = merge(finalData,activityType,by='activityId',all.x=TRUE);

# Updating the colNames vector to include the new column names after merge
colNames  = colnames(finalData); 

# 4. Appropriately label the data set with descriptive activity names. 

# Cleaning up the variable names
for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","_StdDev",colNames[i])
  colNames[i] = gsub("-mean","_Mean",colNames[i])
  colNames[i] = gsub("^(t)","time_",colNames[i])
  colNames[i] = gsub("^(f)","freq_",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity_",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body_",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};

# Assigning the new descriptive column names to the finalData set
colnames(finalData) = colNames;

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

# Creating a new data set without the activityType column
finalDataNoActivityType  = finalData[,names(finalData) != 'activityType'];

# Summarizing the finalDataNoActivityType table to include
#   just the mean of each variable for each activity and each subject
tidyData    = aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean);

# Merging the tidyData with activityType to include descriptive acitvity names
tidyData    = merge(tidyData,activityType,by='activityId',all.x=TRUE);

# Export the tidyData set 
write.table(tidyData, './tidyData.txt',row.names=FALSE,sep='\t');