# You should create one R script called run_analysis.R that does the following:
# 
library(data.table)
library(dplyr)
library(reshape2)
# 1. Merges the training and the test sets to create one data set.
#   Set paths for all relevant files
features_path <- "features.txt" #List of features (col names in X data)
activity_labels_path <- "activity_labels.txt" #Links the class labels (numbers in y data) with their activity name.
subject_train_path <- "train/subject_train.txt" #Identifies the subject who performed the activity
X_train_path <- "train/X_train.txt" #observations for train set
y_train_path <- "train/y_train.txt" #labels for train set
subject_test_path <- "test/subject_test.txt"
X_test_path <- "test/X_test.txt"
y_test_path <- "test/y_test.txt"
output_path <- "tidy_data.txt"

#   Read all relevant files into tables
features_table <- read.table(features_path,comment.char = "")
activity_labels_table <-read.table(activity_labels_path,comment.char = "",colClasses = "factor")
subject_train_table <- read.table(subject_train_path,comment.char = "",colClasses = "factor")
X_train_table <- read.table(X_train_path,comment.char = "",colClasses = "factor")
y_train_table <- read.table(y_train_path,comment.char = "",colClasses = "factor")
subject_test_table <- read.table(subject_test_path,comment.char = "",colClasses = "factor")
X_test_table <- read.table(X_test_path,comment.char = "",colClasses = "numeric")
y_test_table <- read.table(y_test_path,comment.char = "",colClasses = "numeric")

# add names to activity_labels table for merging later
names(activity_labels_table) <- c("activity_id","activity_name")

# create data tables for the test and train data
test_set <- data.table(X_test_table)
train_set <- data.table(X_train_table)

# Get a character vector of feature names from feature_names
feature_names<-as.character(unlist(features_table[2],use.names=FALSE))
# Fix feature names to satisfy the principles of good variable names:
#   - Not have underscores or dots or white spaces
#   - descriptive (replace abbrev with full words)
#   - All lower case when possible (ignoring this because it makes 
#       names unreadable)
#   - Not duplicated

feature_names <- gsub("BodyBody","Body",feature_names)
feature_names <- gsub("(.*)-mean\\(\\)","mean\\1",feature_names)
feature_names <- gsub("(.*)-std\\(\\)","stdev\\1",feature_names)
feature_names <- gsub("fBody","FrequencyBody",feature_names)
feature_names <- gsub("tBody","TimeBody",feature_names)
feature_names <- gsub("tGravity","TimeGravity",feature_names)
feature_names <- gsub("Acc","Accelerometer",feature_names)
feature_names <- gsub("Gyro","Gyroscope",feature_names)
feature_names <- gsub("Mag","Magnitude",feature_names)
feature_names <- gsub("\\-","",feature_names)
feature_names <- gsub("\\.","",feature_names)

# Rename the variables in the data sets with the feature names. Could probably
# do this later but this makes it easier to keep track of the middle steps
 setnames(test_set,old=feature_names)
 setnames(train_set,old=feature_names)

# add columns for subject (based on subject_... tables) and activity (based on
# y_...tables), and create a column that labels all rows with a set 'test' or
# 'train' based on the data set they are from
test_set$subject_id <- subject_test_table[1]
test_set$activity_id <- y_test_table[1]
test_set$setName <- as.factor(rep("test",nrow(test_set)))

train_set$subject_id <- subject_train_table[1]
train_set$activity_id <- y_train_table[1]
train_set$setName <- as.factor(rep("train",nrow(train_set)))

combined_set <- rbind(test_set,train_set)
# 2. Extracts only the measurements on the mean and standard deviation for each
# measurement. Subset out the columns with feature names containing 'mean()' and
# 'std()'
combined_set <- select(combined_set,starts_with("mean"),starts_with("stdev"),subject_id,activity_id,setName)

# 3. Uses descriptive activity names to name the activities in the data set
combined_set <- left_join(x = combined_set,y = activity_labels_table,by = "activity_id")


# 4. Appropriately labels the data set with descriptive variable names. 

# 5. From the data set in step 4, creates a second, independent tidy data set 
#   fix the factor levels in the subject and activity columns
levels(combined_set$subject_id) <- as.character(sort(as.numeric(levels(combined_set$subject_id))))
levels(combined_set$activity_name) <- activity_labels_table$activity_name
#   sort the rows by activity and columns (using the factors fixed above)
combined_set <- arrange(combined_set,activity_name,subject_id)
# Put the categorical tables on the far left, remove activity ID column as it
# has been replaced with activity name
combined_set <- select(combined_set,activity_name,subject_id,setName,starts_with("mean"),starts_with("stdev"))

# Write tidy table to file
write.table(combined_set,output_path,row.name=FALSE)
# Verify by opening and viewing 
View(read.table(output_path,header = TRUE))