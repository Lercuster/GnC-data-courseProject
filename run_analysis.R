
library(dplyr)

########################################################

# step 1: download zip, unzip it 

########################################################


file_name = "course_project.zip"
dest_file = paste0(".\\data\\", file_name)

if (!file.exists(dest_file)){
  file_url = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(file_url, dest_file)
} else {
  print("zip already exists!")
}

if (!file.exists(".\\data\\UCI HAR Dataset")){
  unzip(dest_file, exdir = ".\\data")
} else {
  print("file already exists!")
}

dest_file = ".\\data\\UCI HAR Dataset\\"

########################################################

# step 2: read data

########################################################

testSubjects = read.table(".\\data\\UCI HAR Dataset\\test\\subject_test.txt")
testX_test = read.table(paste0(dest_file, "test\\X_test.txt"))
testY_test = read.table(paste0(dest_file, "test\\y_test.txt"))

trainingSubjects = read.table(paste0(dest_file, "train\\subject_train.txt"))
trainingX_train = read.table(paste0(dest_file, "train\\X_train.txt"))
trainingY_train = read.table(paste0(dest_file, "train\\y_train.txt"))

activity_labels = read.table(paste0(dest_file, "activity_labels.txt"))
features = read.table(paste0(dest_file, "features.txt"))
features[,2] <- as.character(features[,2])

########################################################

# step 3: merge all data in one data set called merged_data

########################################################

merged_dataset =rbind(
  cbind(trainingSubjects, trainingX_train, trainingY_train),
  cbind(testSubjects, testX_test, testY_test)
)

colnames(merged_dataset) = c("Subject", features[ , 2],  "Activity")

########################################################

# step 4: extract only the measurements on the mean and 
#          standard deviation for each measurement

########################################################

col_to_keep = grepl("Subject|Activity|mean|std", colnames(merged_dataset))
merged_dataset = merged_dataset[ , col_to_keep]

merged_dataset$Activity = factor(merged_dataset$Activity,
                                 levels = activity_labels[, 1], labels = activity_labels[ , 2])

########################################################

#step 5: appropriately label the data set with descriptive variable names

########################################################

merged_dataset_names = colnames(merged_dataset)
merged_dataset_names = gsub("[-()]", "", merged_dataset_names)
merged_dataset_names = gsub("BodyBody", 'Body', merged_dataset_names)
merged_dataset_names = gsub("mean", "Mean", merged_dataset_names)
merged_dataset_names = gsub("std", "Std", merged_dataset_names)
colnames(merged_dataset) = merged_dataset_names

########################################################

#step 6: create new tidy data set with the average of 
#       each variable for each activity and each subject

########################################################

merged_dataset_mean <- merged_dataset %>%
  group_by(Subject, Activity) %>%
  summarise_each(funs(mean))

write.table(merged_dataset_mean, "tidy_data_set.txt", row.names = FALSE, 
            quote = FALSE)

print("DONE")