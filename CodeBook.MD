{\rtf1\ansi\ansicpg1252\cocoartf1265\cocoasubrtf190
{\fonttbl\f0\fnil\fcharset0 HelveticaNeue;}
{\colortbl;\red255\green255\blue255;\red38\green38\blue38;\red38\green38\blue38;\red255\green255\blue255;
}
{\*\listtable{\list\listtemplateid1\listhybrid{\listlevel\levelnfc0\levelnfcn0\leveljc0\leveljcn0\levelfollow0\levelstartat1\levelspace360\levelindent0{\*\levelmarker \{decimal\}.}{\leveltext\leveltemplateid1\'02\'00.;}{\levelnumbers\'01;}\fi-360\li720\lin720 }{\listname ;}\listid1}
{\list\listtemplateid2\listhybrid{\listlevel\levelnfc23\levelnfcn23\leveljc0\leveljcn0\levelfollow0\levelstartat1\levelspace360\levelindent0{\*\levelmarker * }{\leveltext\leveltemplateid101\'02* ;}{\levelnumbers;}\fi-360\li720\lin720 }{\listname ;}\listid2}}
{\*\listoverridetable{\listoverride\listid1\listoverridecount0\ls1}{\listoverride\listid2\listoverridecount0\ls2}}
\margl1440\margr1440\vieww10800\viewh8400\viewkind0
\deftab720
\pard\pardeftab720\sl420

\f0\fs28 \cf2 Tidy HAR Code Book\
======================\
\
This codebook highlights how the Human Activity Recognition Using Smartphones data set. For a walkthrough of the script, please visit the README.md file. \
\
## Transformations\
This data set was tidied according to the following instructions copied from the Jeff Leek\'92s Getting and Cleaning Data class on Coursera:\
\pard\tx220\tx720\pardeftab720\li720\fi-720\sl420
\ls1\ilvl0\cf3 \cb4 {\listtext	1.	}Merges the training and the test sets to create one data set.\
{\listtext	2.	}Extracts only the measurements on the mean and standard deviation for each measurement.\'a0\
{\listtext	3.	}Uses descriptive activity names to name the activities in the data set\
{\listtext	4.	}Appropriately labels the data set with descriptive activity names.\'a0\
{\listtext	5.	}Creates a second, independent tidy data set with the average of each variable for each activity and each subject.\'a0\
\pard\pardeftab720\sl420
\cf2 \cb1 \
## Data\
From herein, the tidied data set will be referred to as Tidy_HAR. The following description of the data set is summarized from the original Readme.txt file from the original data files. \
\
The data from the original file includes observations of 30 people performing six different activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) while wearing a Samsung Galaxy S II. The accelerometer and gyroscope on the Samsung phone captured acceleration and velocity data over three axes. \
\
#Variables and Observations\
Tidy_HAR has 48 observations of 180 variables. Each variable is named after the subject (denoted by X and a number) and activity performed by the subject. For example, the variable name for Subject 3 sitting is \'91X3.SITTING\'92. Observations (or row names) for each variable are as follows:\
\
\pard\tx220\tx720\pardeftab720\li720\fi-720\sl420
\ls2\ilvl0\cf2 {\listtext	* 	}tBodyAcc-mean()-X\
{\listtext	* 	}tBodyAcc-mean()-Y\
{\listtext	* 	}tBodyAcc-mean()-Z\
{\listtext	* 	}tBodyAcc-std()-X\
{\listtext	* 	}tBodyAcc-std()-Y\
{\listtext	* 	}tBodyAcc-std()-Z\
{\listtext	* 	}tGravityAcc-mean()-X\
{\listtext	* 	}tGravityAcc-mean()-Y\
{\listtext	* 	}tGravityAcc-mean()-Z\
{\listtext	* 	}tGravityAcc-std()-X\
{\listtext	* 	}tGravityAcc-std()-Y\
{\listtext	* 	}tGravityAcc-std()-Z\
{\listtext	* 	}tBodyAccJerk-mean()-X\
{\listtext	* 	}tBodyAccJerk-mean()-Y\
{\listtext	* 	}tBodyAccJerk-mean()-Z\
{\listtext	* 	}tBodyAccJerk-std()-X\
{\listtext	* 	}tBodyAccJerk-std()-Y\
{\listtext	* 	}tBodyAccJerk-std()-Z\
{\listtext	* 	}tBodyGyro-mean()-X\
{\listtext	* 	}tBodyGyro-mean()-Y\
{\listtext	* 	}tBodyGyro-mean()-Z\
{\listtext	* 	}tBodyGyro-std()-X\
{\listtext	* 	}tBodyGyro-std()-Y\
{\listtext	* 	}tBodyGyro-std()-Z\
{\listtext	* 	}tBodyGyroJerk-mean()-X\
{\listtext	* 	}tBodyGyroJerk-mean()-Y\
{\listtext	* 	}tBodyGyroJerk-mean()-Z\
{\listtext	* 	}tBodyGyroJerk-std()-X\
{\listtext	* 	}tBodyGyroJerk-std()-Y\
{\listtext	* 	}tBodyGyroJerk-std()-Z\
{\listtext	* 	}fBodyAcc-mean()-X\
{\listtext	* 	}fBodyAcc-mean()-Y\
{\listtext	* 	}fBodyAcc-mean()-Z\
{\listtext	* 	}fBodyAcc-std()-X\
{\listtext	* 	}fBodyAcc-std()-Y\
{\listtext	* 	}fBodyAcc-std()-Z\
{\listtext	* 	}fBodyAccJerk-mean()-X\
{\listtext	* 	}fBodyAccJerk-mean()-Y\
{\listtext	* 	}fBodyAccJerk-mean()-Z\
{\listtext	* 	}fBodyAccJerk-std()-X\
{\listtext	* 	}fBodyAccJerk-std()-Y\
{\listtext	* 	}fBodyAccJerk-std()-Z\
{\listtext	* 	}fBodyGyro-mean()-X\
{\listtext	* 	}fBodyGyro-mean()-Y\
{\listtext	* 	}fBodyGyro-mean()-Z\
{\listtext	* 	}fBodyGyro-std()-X\
{\listtext	* 	}fBodyGyro-std()-Y\
{\listtext	* 	}fBodyGyro-std()-Z\cf3 \cb4 \
\pard\tx720\pardeftab720\sl420
\cf3 \
}