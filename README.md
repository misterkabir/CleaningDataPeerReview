CleaningDataPeerReview
======================

This is a program created to tidy data for a class on Coursera. Data was originally sourced from www.smartlab.ws and can be accessed here: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip. Data was accessed on 27 April 2014. Please see acknowledgements at the end of this document. 

##(0) The tidy process requires the `Hmisc` and `plyr` package.

```S
require(Hmisc)
require(plyr)
```

##(1) We first load data and store to individual dataframes using the `read.table` function.

```S
X_test <- read.table("test/X_test.txt")
Y_test <- read.table("test/y_test.txt")
Subject_test <- read.table("test/subject_test.txt")
X_train <- read.table("train/X_train.txt")
Y_train <- read.table("train/y_train.txt")
Subject_train <- read.table("train/subject_train.txt")
```

##(2) This script takes the relevant attribute names (features) from the second column of featirest.txt and adds it to the X dataframes. Then variable names in Y and Subject dataframes are added to reflect "Activty" and "ID".

```S
features <- read.table("features.txt")
features <- features[,2]
names(X_test) <- features
names(X_train) <- features
names(Y_test) <- "Activity"
names(Y_train) <- "Activity"
names(Subject_test) <- "ID"
names(Subject_train) <- "ID"
```

##(3) Next we combine the X and Y data to form a single respective dataframe; one for test and one for train.

```S
test_df <- cbind(Subject_test, Y_test, X_test)
train_df <- cbind(Subject_train, Y_train, X_train)
```

##(4) Then we append both data frames into one holistic dataframe that can be used for analysis.

```S
HAR_df <- rbind(test_df, train_df)
```

##(5) Codes reflecting description names for each activity are replaced with the actual activity names to improve readbility.

```S
as.numeric(HAR_df$Activity)
activityLab <- read.table("activity_labels.txt")
attach(HAR_df)
HAR_df$Activity[Activity == 1] <- "WALKING"
HAR_df$Activity[Activity == 2] <- "WALKING_UPSTAIRS"
HAR_df$Activity[Activity == 3] <- "WALKING_DOWNSTAIRS"
HAR_df$Activity[Activity == 4] <- "SITTING"
HAR_df$Activity[Activity == 5] <- "STANDING"
HAR_df$Activity[Activity == 6] <- "LAYING"
detach(HAR_df)
```

##(6) Here we analyze means for columns matching "means" and "standard deviation" in variable names.

```S
toMatch <- c("mean\\(\\)[-]", "std\\(\\)[-]")
matches <- unique(grep(paste(toMatch,collapse="|"), features, value=TRUE))
HAR_df1 <- cbind(HAR_df[,1:2], HAR_df[,matches]) ## Working file
s <- split(HAR_df1, list(HAR_df1$ID, HAR_df1$Activity))
Tidy_HARt <- sapply(s, function(x) colMeans(x[,matches]))
Tidy_HAR <- data.frame(t(Tidy_HARt))
```

##(7) (Optional Step) Write tidy data by removing hashtags from following line:

```S
write.table(Tidy_HAR, file = "Tidy_HAR.txt")
```


##The End. 

##Acknowledgements:

[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012

A full description of the data can be found here: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 
