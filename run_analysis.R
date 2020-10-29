#### Step 0. Downloading and unzipping dataset ####
# Load dplyr library
library(dplyr)
# creating directory
if (!file.exists("./data")){
      dir.create("./data")
}
# URL for the project data:
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile = "./data/Dataset.zip")

# unzip the dataset to into the directory
unzip(zipfile = "./data/Dataset.zip",exdir = "./data")

#### Step 1.Merges the training and the test sets to create one data set ####

# 1.1 Reading all files and assigning names to cols

features <- read.table("./data/UCI HAR Dataset/features.txt" ,col.names = c("n","functions"))
activities <- read.table("./data/UCI HAR Dataset/activity_labels.txt", col.names = c("activityId", "activity"))
subject_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt", col.names = "subjectId")
x_test <- read.table("./data/UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("./data/UCI HAR Dataset/test/y_test.txt", col.names = "activityId")
subject_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt", col.names = "subjectId")
x_train <- read.table("./data/UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("./data/UCI HAR Dataset/train/y_train.txt", col.names = "activityId")


#1.2 Merging all data in one set:

temp1 <- cbind(subject_train, y_train ,x_train)
temp2 <- cbind(subject_test, y_test,x_test)
Merged <- rbind(temp1, temp2)

#### Step 2 Extracts only the measurements on the mean and standard deviation for each measurement ####

# Using dplyr to select the columns of Merged dataframe to get the needed cols:

ExData <- Merged%>%select(subjectId,activityId,contains("mean"),contains("std")) 



#### Step 3. Uses descriptive activity names to name the activities in the data set ####

ExData$activityId <- activities[ExData$activityId, 2]

#### Step 4. Appropriately labels the data set with descriptive variable names ####

names(ExData)[2] = "activity"
names(ExData)<-gsub("Acc", "Accelerometer", names(ExData))
names(ExData)<-gsub("Gyro", "Gyroscope", names(ExData))
names(ExData)<-gsub("BodyBody", "Body", names(ExData))
names(ExData)<-gsub("Mag", "Magnitude", names(ExData))
names(ExData)<-gsub("^t", "Time", names(ExData))
names(ExData)<-gsub("^f", "Frequency", names(ExData))
names(ExData)<-gsub("tBody", "TimeBody", names(ExData))
names(ExData)<-gsub("-mean()", "Mean", names(ExData), ignore.case = TRUE)
names(ExData)<-gsub("-std()", "STD", names(ExData), ignore.case = TRUE)
names(ExData)<-gsub("-freq()", "Frequency", names(ExData), ignore.case = TRUE)
names(ExData)<-gsub("angle", "Angle", names(ExData))
names(ExData)<-gsub("gravity", "Gravity", names(ExData))

#### Step 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject ####

TidyDat <- ExData %>% group_by(subjectId,activity)%>% 
      summarise_all(.funs = mean)
write.table(TidyDat,"TidyDat.txt",row.names = FALSE)      

