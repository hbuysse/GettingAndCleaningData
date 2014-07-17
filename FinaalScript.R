###################################################
## Courseproject for 'Getting and Cleaning Data' ##
## Heidi Buysse                                  ##
## July 2014                                     ##
# #################################################

# the purpose of the project: You should create one R script called run_analysis.R that does the following. 

# 1) Merges the training and the test sets to create one data set.
# 2) Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3) Uses descriptive activity names to name the activities in the data set
# 4° Appropriately labels the data set with descriptive variable names. 
# 5) Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 


#set working directory to the location where the UCI HAR Dataset was unzipped;
# this working directory has to be changed to our own!
setwd("C:/Heidi/Coursera/GettingAndCleaningData_home/project/data");

# The zip-file has been downloaded en has been unzipped in a specific folder.
# when reading the readme-file, it has been clear not all files in the ZIP-file are necessary.
file_list <- list.files()
file_list
# the files that should be in one directory are to further continue this script are:
# "activity_labels.txt" "features.txt"        "features_info.txt"  
# "README.txt"          "subject_test.txt"    "subject_train.txt"  
# "X_test.txt"          "X_train.txt"        
# "y_test.txt"          "y_train.txt"  


# 1. Merges the training and the test sets to create one data set.
# features and activity labels have labels we need further in the exercise
# Read in the data from files
features = read.table('./features.txt',header=FALSE); #imports features.txt
activityType = read.table('./activity_labels.txt',header=FALSE); #imports activity_labels.txt
# read in data from the training
subjectTrain = read.table('./subject_train.txt',header=FALSE); #imports subject_train.txt
xTrain = read.table('./x_train.txt',header=FALSE); #imports x_train.txt
yTrain = read.table('./y_train.txt',header=FALSE); #imports y_train.txt

# Assign column names to the data that has been imported before
colnames(activityType) = c('activityId','activityType');
colnames(subjectTrain) = "subjectId";
colnames(xTrain) = features[,2]; # the first column we do not need
colnames(yTrain) = "activityId";

# We have to create the final training set by merging the different training files:
# yTrain, subjectTrain, and xTrain
trainingData = cbind(yTrain,subjectTrain,xTrain);

# We have to do the same steps for the test data
# Read in the test data
subjectTest = read.table('./subject_test.txt',header=FALSE); #import subject_test.txt
xTest = read.table('./x_test.txt',header=FALSE); #import x_test.txt
yTest = read.table('./y_test.txt',header=FALSE); #import y_test.txt

# Assign column names to the test data that has been imported before
colnames(subjectTest) = "subjectId";
colnames(xTest) = features[,2]; # we do not need the first column
colnames(yTest) = "activityId";


# Create the final test set by merging the xTest, yTest and subjectTest data
testData = cbind(yTest,subjectTest,xTest);


# The training and test data has to be combined
totalData = rbind(trainingData,testData);

# we do not need all the data, but just the data about the mean and STDDEV
# We have to create a vector for the column names from the totalData
# this will be used to select the desired mean() & stddev() columns
colNames = colnames(totalData);

# 2. Extract only the measurements on the mean and standard deviation for each measurement.
# it is important to keep also ID information
# Create a logicalVector that contains TRUE values for the ID, mean() & stddev() columns and FALSE for others
logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));

# we subset the totalData table based on the logicalVector 
# we will only keep the desired columns
totalData = totalData[logicalVector==TRUE];

# 3. Use descriptive activity names to name the activities in the data set
# descriptive names for the activities can be found in activityType; we have to merge both
totalData = merge(totalData,activityType,by='activityId',all.x=TRUE);

# Updating the colNames vector to include the new column names after merge
colNames = colnames(totalData);

# 4. Appropriately label the data set with descriptive activity names.

# Cleaning up the variable names
for (i in 1:length(colNames))
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};

# Reassigning the new descriptive column names to the totalData set
colnames(totalData) = colNames;

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject.

# Create a new table, totalDataNoActivityType without the activityType column
totalDataNoActivityType = totalData[,names(totalData) != 'activityType'];

# Summarizing the finalDataNoActivityType table to include just the mean of each variable for each activity and each subject
tidyData = aggregate(totalDataNoActivityType[,names(totalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=totalDataNoActivityType$activityId,subjectId = totalDataNoActivityType$subjectId),mean);

# Merging the tidyData with activityType to include descriptive acitvity names
tidyData = merge(tidyData,activityType,by='activityId',all.x=TRUE);

# Export the tidyData set
write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t');
