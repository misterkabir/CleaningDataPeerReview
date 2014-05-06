##This is a program created to tidy data for a class on Coursera. 
require(Hmisc)
require(plyr)

## Step 1:
## Load data into individual dataframes
X_test <- read.table("test/X_test.txt")
Y_test <- read.table("test/y_test.txt")
Subject_test <- read.table("test/subject_test.txt")
X_train <- read.table("train/X_train.txt")
Y_train <- read.table("train/y_train.txt")
Subject_train <- read.table("train/subject_train.txt")

## Step 2:
## Load and set variable labels
features <- read.table("features.txt")
features <- features[,2]
names(X_test) <- features
names(X_train) <- features
names(Y_test) <- "Activity"
names(Y_train) <- "Activity"
names(Subject_test) <- "ID"
names(Subject_train) <- "ID"

## Step 3:
## Combine X and Y data to form a single respective dataframe; one for test and one for train
test_df <- cbind(Subject_test, Y_test, X_test)
train_df <- cbind(Subject_train, Y_train, X_train)

## Step 4:
## Append both data frames into one holistic dataframe
HAR_df <- rbind(test_df, train_df)

## Step 5:
## Change Descriptions labels
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

## Step 6:
## Analyze means for columns matching means and standard deviation
toMatch <- c("mean\\(\\)[-]", "std\\(\\)[-]")
matches <- unique(grep(paste(toMatch,collapse="|"), features, value=TRUE))
HAR_df1 <- cbind(HAR_df[,1:2], HAR_df[,matches]) ## Working file
s <- split(HAR_df1, list(HAR_df1$ID, HAR_df1$Activity))
Tidy_HARt <- sapply(s, function(x) colMeans(x[,matches]))
Tidy_HAR <- data.frame(t(Tidy_HARt))

## Step 7: (Optional)
## Write tidy data by removing hashtags from following line:
write.table(Tidy_HAR, file = "Tidy_HAR.txt")