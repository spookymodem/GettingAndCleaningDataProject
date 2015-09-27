# first, read in the features
features <- read.table("UCI HAR Dataset/features.txt", as.is = c(2), col.names = c("index", "feature_name"))

# then read in training data (assumes working directory is set properly)
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$feature_name)
y_train <- read.table("UCI HAR Dataset/train/Y_train.txt", col.names = c("activity_num"))
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = c("subject_num"))

# next, read in test data
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$feature_name)
y_test <- read.table("UCI HAR Dataset/test/Y_test.txt", col.names = c("activity_num"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = c("subject_num"))

# load the dplyr library
library(dplyr)

# create a features subset that only contains the features that include 'mean' or 'std'
features_mean_std <- filter(features, grepl("(mean|std)", features$feature_name))

# combine training and test; only include x values for features that include 'mean' or 'std'
x <- rbind(x_train[features_mean_std$index], x_test[features_mean_std$index])
y <- rbind(y_train, y_test)
subject <- rbind(subject_train, subject_test)

# create a vector that contains the activity names
activity_name <- ifelse(y$activity_num == 1, "WALKING",
                    ifelse(y$activity_num == 2, "WALKING_UPSTAIRS",
                      ifelse(y$activity_num == 3, "WALKING_DOWNSTAIRS",
                        ifelse(y$activity_num == 4, "SITTING",
                          ifelse(y$activity_num == 5, "STANDING", "LAYING")))))

# combine all the data into one data set
data <- cbind(x, y, activity_name, subject)

# create a second, independent tidy data set with the average of each variable for each activity and each subject
average_per_activity <- c()
average_per_subject <-  c()

# use a for loop to compute the averages per activity and averages per subject
for (i in 1:79) {
  average_per_activity <- rbind(average_per_activity,tapply(data[,i], data$activity_num, mean))
  average_per_subject <-  rbind(average_per_subject, tapply(data[,i], data$subject, mean))
}

# set the column names for the tidy data set
colnames(average_per_activity) <- c("a1_avg", "a2_avg", "a3_avg", "a4_avg", "a5_avg", "a6_avg")
colnames(average_per_subject)  <- c("s1_avg", "s2_avg", "s3_avg", "s4_avg", "s5_avg", "s6_avg", "s7_avg", "s8_avg", "s9_avg", "s10_avg",
                                    "s11_avg", "s12_avg", "s13_avg", "s14_avg", "s15_avg", "s16_avg", "s17_avg", "s18_avg", "s19_avg", "s20_avg",
                                    "s21_avg", "s22_avg", "s23_avg", "s24_avg", "s25_avg", "s26_avg", "s27_avg", "s28_avg", "s29_avg", "s30_avg")

# create the tidy data set
tidy_data <- cbind(average_per_activity, average_per_subject)

# name the rows in the tidy data set
rownames(tidy_data) <- features_mean_std$feature_name

# wrtie the tidy data to a file
write.table(tidy_data, file = "tidy_data.txt", row.names = FALSE)
