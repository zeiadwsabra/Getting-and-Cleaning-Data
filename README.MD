#Getting and Cleaning Data - Assignment
This repo holds the scripts and output for the peer-reviewed assignment of the Getting and Cleaning Data course on Coursera
##Files
###run_analysis.R
Runs on the Human Activity Recognition Using Smartphones Data Set.
####Steps
* Reads training and testing data and combine them togther.
	* using the readDataSet function to  read training and testing files  
* Appropriately labels the data set with descriptive variable names.
	* In the readDataSet function, it Reads features.txt and labels the columns.
* Uses descriptive activity names to name the activities in the data set.
	* In the readDataSet function, it Reads activity_labels.txt and maps activity ids to descriptive name.
* Extracts only the measurements on the mean and standard deviation for each measurement.
	* Using A regular expression to select all columns containing the word mean or std to filter the columns
	* It then adds both subject and activity columns to the selected columns  
* Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
	* Using aggregate functiong to comuput the mean of all the filtered columns in previous step for each (subject, activity)

###Cookbook.md
Contains variable names and references for original data set.

