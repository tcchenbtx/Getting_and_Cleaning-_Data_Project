## run_analysis.R does the following:
## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for 
##    each measurement. 
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive variable names. 
## 5. From the data set in step 4, creates a second, independent tidy data set 
##    with the average of each variable for each activity and each subject.
##

# 1. load needed packages: dplyr and reshape2

if (!require("dplyr")) {
    install.packages("dplyr")
}

if (!require("reshape2")) {
    install.packages("reshape2")
}

library(dplyr)
library(reshape2)

# 2. load data
x_train <- read.table('./UCI HAR Dataset/train/X_train.txt')
y_train <- read.table('./UCI HAR Dataset/train/y_train.txt')
x_test <- read.table('./UCI HAR Dataset/test/X_test.txt')
y_test <- read.table('./UCI HAR Dataset/test/y_test.txt')
train_subject <- read.table('./UCI HAR Dataset/train/subject_train.txt')
test_subject <- read.table('./UCI HAR Dataset/test/subject_test.txt')

# 3. load activity and feature names
features <- read.table("./UCI HAR Dataset/features.txt",stringsAsFactors=FALSE)
activities <- read.table("./UCI HAR Dataset/activity_labels.txt", stringsAsFactors=FALSE)

# 4. rename varialbe with appropriate name
r_train_suject <- rename(train_subject, subject_ID = V1)
r_test_suject <- rename(test_subject, subject_ID = V1)
r_train_activity <- rename(y_train, activity_ID = V1)
r_test_activity <- rename(y_test, activity_ID = V1)
names(x_test) <- features[,2]
names(x_train) <- features[,2]

# 5. add activity names

for (i in 1:nrow(activities)){
    r_train_activity[r_train_activity$activity_ID==i,"activity_name"] = activities[i,"V2"]
}

for (i in 1:nrow(activities)){
    r_test_activity[r_test_activity$activity_ID==i,"activity_name"] = activities[i,"V2"]
}

# 6. get extract features
extract_features <- grepl("mean|std", features[,2])
new_x_test <- x_test[,extract_features]
new_x_train <- x_train[,extract_features]

# 7. merge training and test sets

train_total <- cbind(r_train_suject, r_train_activity, new_x_train)
test_total <- cbind(r_test_suject, r_test_activity, new_x_test)
all_data <- rbind(test_total,train_total)

# 8. create tidy data set (new_tidy_data.txt)with the average of each variable for each activity and each subject
measure_variable <- names(all_data)[4:ncol(all_data)]
melt_data <- melt(all_data, id=c("subject_ID","activity_name"), measure.vars = measure_variable)

new_tidy <- dcast(melt_data, subject_ID + activity_name ~ variable, mean)
write.table(new_tidy, file="./new_tidy_data.txt", row.names = FALSE)

