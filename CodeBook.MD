Tidy HAR Code Book
======================

This codebook highlights how the Human Activity Recognition Using Smartphones data set. For a walkthrough of the script, please visit the README.md file. 

## Transformations
This data set was tidied according to the following instructions copied from the Jeff Leek’s Getting and Cleaning Data class on Coursera:
	1.	Merges the training and the test sets to create one data set.
	2.	Extracts only the measurements on the mean and standard deviation for each measurement. 
	3.	Uses descriptive activity names to name the activities in the data set
	4.	Appropriately labels the data set with descriptive activity names. 
	5.	Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

## Data
From herein, the tidied data set will be referred to as Tidy_HAR. The following description of the data set is summarized from the original Readme.txt file from the original data files. 

The data from the original file includes observations of 30 people performing six different activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) while wearing a Samsung Galaxy S II. The accelerometer and gyroscope on the Samsung phone captured acceleration and velocity data over three axes. 

#Variables and Observations
Tidy_HAR has 48 observations of 180 variables. Each variable is named after the subject (denoted by X and a number) and activity performed by the subject. For example, the variable name for Subject 3 sitting is ‘X3.SITTING’. Observations (or row names) for each variable are as follows:

	* 	tBodyAcc-mean()-X
	* 	tBodyAcc-mean()-Y
	* 	tBodyAcc-mean()-Z
	* 	tBodyAcc-std()-X
	* 	tBodyAcc-std()-Y
	* 	tBodyAcc-std()-Z
	* 	tGravityAcc-mean()-X
	* 	tGravityAcc-mean()-Y
	* 	tGravityAcc-mean()-Z
	* 	tGravityAcc-std()-X
	* 	tGravityAcc-std()-Y
	* 	tGravityAcc-std()-Z
	* 	tBodyAccJerk-mean()-X
	* 	tBodyAccJerk-mean()-Y
	* 	tBodyAccJerk-mean()-Z
	* 	tBodyAccJerk-std()-X
	* 	tBodyAccJerk-std()-Y
	* 	tBodyAccJerk-std()-Z
	* 	tBodyGyro-mean()-X
	* 	tBodyGyro-mean()-Y
	* 	tBodyGyro-mean()-Z
	* 	tBodyGyro-std()-X
	* 	tBodyGyro-std()-Y
	* 	tBodyGyro-std()-Z
	* 	tBodyGyroJerk-mean()-X
	* 	tBodyGyroJerk-mean()-Y
	* 	tBodyGyroJerk-mean()-Z
	* 	tBodyGyroJerk-std()-X
	* 	tBodyGyroJerk-std()-Y
	* 	tBodyGyroJerk-std()-Z
	* 	fBodyAcc-mean()-X
	* 	fBodyAcc-mean()-Y
	* 	fBodyAcc-mean()-Z
	* 	fBodyAcc-std()-X
	* 	fBodyAcc-std()-Y
	* 	fBodyAcc-std()-Z
	* 	fBodyAccJerk-mean()-X
	* 	fBodyAccJerk-mean()-Y
	* 	fBodyAccJerk-mean()-Z
	* 	fBodyAccJerk-std()-X
	* 	fBodyAccJerk-std()-Y
	* 	fBodyAccJerk-std()-Z
	* 	fBodyGyro-mean()-X
	* 	fBodyGyro-mean()-Y
	* 	fBodyGyro-mean()-Z
	* 	fBodyGyro-std()-X
	* 	fBodyGyro-std()-Y
	* 	fBodyGyro-std()-Z

