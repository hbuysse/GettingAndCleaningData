## Coursera Getting and Cleaning Data Project by Heidi Buysse


### Project description
We should “a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md”.

### Source Data

A full description of the data is available at the site where the data was obtained: 

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

The data for the project can be found at: 

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

Reference to the dataset:
License:
========
Use of this dataset in publications must be acknowledged by referencing the following publication [1] 

[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012

This dataset is distributed AS-IS and no responsibility implied or explicit can be addressed to the authors or their institutions for its use or misuse. Any commercial use is prohibited.

Jorge L. Reyes-Ortiz, Alessandro Ghio, Luca Oneto, Davide Anguita. November 2012.


### Data Set Information
An experiment has been set up with N=30 (ages ranged between 19 and 48 years). Each person performed six activities: walking, walking upstairs, walking downstairs, sitting, standing, and laying). During these activities, they wear a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, they captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. This dataset has been randomly partitioned into two datasets: N=21 (70%) for the training dataset and N=9 (30%) for the test dataset.

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.


### Attribute Information
For each record in the dataset it is provided: 
- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration. 
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

### course project description
You should create one R script called run_analysis.R that does the following: 
-	Merges the training and the test sets to create one data set.
-	Extracts only the measurements on the mean and standard deviation for each measurement. 
-	Uses descriptive activity names to name the activities in the data set
-	Appropriately labels the data set with descriptive variable names. 
-	Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 


### Part 1. Merges the training and the test sets to create one data set.
After downloading and unzipping the data, the source directory should be set right [line 18 in the R-script]. All the necessary files should be positioned into this working directory
- features.txt
- activity_labels.txt
- subject_train.txt
- x_train.txt
- y_train.txt
- subject_test.txt
- x_test.txt
- y_test.txt

Column names has to be assigned and both datasets (training and test) ha to be merged to create one data set.

## Part 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
We first have to create a logical vector that contains TRUE values for the ID, mean and stdev columns and FALSE values for the others.
Subsequently, this data has to be subsetted to keep only the necessary columns.

## Part 3. Uses descriptive activity names to name the activities in the data set
The table activiType includes the necessary information. So, the data subset has to be merged with this table to include the descriptive activity names

## Part 4. Appropriately labels the data set with descriptive activity names.
The gsub-function does that.
gsub(pattern, replacement, x) with
-	pattern – A pattern to search for, which is assumed to be a regular expression. Use an additional argument fixed=TRUE to look for a pattern without using regular expressions.
-	replacement – A character string to replace the occurrence (or occurrences for gsub) of pattern.
-	x – A character vector to search for pattern. Each element will be searched separately.

## Part 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 
This tidy data set has been written to a new dataset tidyData, as a txt file which could be easily imported into R.

