library(dplyr)

filename <- "Course_Project.zip"

# Download the dataset and Check if archive already exists
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, filename, method="curl")
}  

 if (!file.exists("UCI HAR Dataset")) { 
  unzip(filename) 
}

#Create data frame for each dataset
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activity <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("ID", "activity"))
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "ID")
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "ID")
subject <- read.table("UCI HAR Dataset/test/subject.txt", col.names = "subject")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")



#Step 1: Merges the training and the test sets to create one data set
X <- rbind(x_train, x_test)
Y <- rbind(y_train, y_test)
Subject <- rbind(subject_train, subject)
Merged_Dataset <- cbind(Subject, Y, X)

#Step 2: Extracts only the measurements on the mean and standard deviation for each measurement
Tidy_Dataset <- Merged_Dataset %>% select(subject, ID, contains("mean"), contains("std"))

#Step 3: Uses descriptive activity names to name the activities in the data set
Tidy_Dataset$ID <- activity[Tidy_Dataset$ID, 2]

#Step4: Appropriately labels the data set with descriptive variable names
names(Tidy_Dataset)[2] = "activity"
names(Tidy_Dataset)<-gsub("Acc", "Accelerometer", names(Tidy_Dataset))
names(Tidy_Dataset)<-gsub("Gyro", "Gyroscope", names(Tidy_Dataset))
names(Tidy_Dataset)<-gsub("BodyBody", "Body", names(Tidy_Dataset))
names(Tidy_Dataset)<-gsub("Mag", "Magnitude", names(Tidy_Dataset))
names(Tidy_Dataset)<-gsub("^t", "Time", names(Tidy_Dataset))
names(Tidy_Dataset)<-gsub("^f", "Frequency", names(Tidy_Dataset))
names(Tidy_Dataset)<-gsub("tBody", "TimeBody", names(Tidy_Dataset))
names(Tidy_Dataset)<-gsub("-mean()", "Mean", names(Tidy_Dataset), ignore.case = TRUE)
names(Tidy_Dataset)<-gsub("-std()", "STD", names(Tidy_Dataset), ignore.case = TRUE)
names(Tidy_Dataset)<-gsub("-freq()", "Frequency", names(Tidy_Dataset), ignore.case = TRUE)
names(Tidy_Dataset)<-gsub("angle", "Angle", names(Tidy_Dataset))
names(Tidy_Dataset)<-gsub("gravity", "Gravity", names(Tidy_Dataset))

#Step 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject
Final_Result <- Tidy_Dataset %>% group_by(subject, activity) %>% summarise_all(funs(mean))
write.table(Final_Result, "Final_Result.txt", row.name=FALSE)

