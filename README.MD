## Getting and Cleaning Data Project by Heidi Buysse
This files describes the repository for the course project Getting and Cleaning data
## Overview
Before one can start performing statistical analysis onto datasets, it is important to know how to put data in a ‘right’ format. This course project has the objective to practice this. It was hard to understand the different files into the zip-file, but it was an interesting learning project.
A full description of the data is available at the site where the data was obtained: 

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

The data for the project can be found at: 

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
## Before one can run the script, it is important to make some modifications to the script
After downloading and unzipping the data, the source directory should be set right [line 18 in the R-script]. All the necessary files should be positioned into this working directory
- features.txt
- activity_labels.txt
- subject_train.txt
- x_train.txt
- y_train.txt
- subject_test.txt
- x_test.txt
- y_test.txt
## Project Summary
The description of the projects was:
You should create one R script called run_analysis.R that does the following: 
-	Merges the training and the test sets to create one data set.
-	Extracts only the measurements on the mean and standard deviation for each measurement. 
-	Uses descriptive activity names to name the activities in the data set
-	Appropriately labels the data set with descriptive variable names. 
-	Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 


## Further information
Further information can be found in the CodeBook.MD file
The script to run calls run_analysis.R 
