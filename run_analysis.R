
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
filename <- "getdata_dataset.zip"
download.file(fileURL, filename, mode = "wb")
setwd("~/Getting-and-Cleaning-Data-Course-Project/UCI HAR Dataset")
list.files()

# read tahe train data

features     <- read.table('./features.txt',header=FALSE); #imports features.txt
activityType <- read.table('./activity_labels.txt',header=FALSE); #imports activity_labels.txt
subjectTrain <- read.table('./train/subject_train.txt',header=FALSE); #imports subject_train.txt
xTrain       <- read.table('./train/x_train.txt',header=FALSE); #imports x_train.txt
yTrain       <- read.table('./train/y_train.txt',header=FALSE); #imports y_train.txt

# Assigin column names to the data imported above
colnames(activityType)  = c('activityId','activityType')
colnames(subjectTrain)  = "subjectId"
colnames(xTrain)        = features[,2] 
colnames(yTrain)        = "activityId"

# cCreate the final training set by merging yTrain, subjectTrain, and xTrain
trainingData = cbind(yTrain,subjectTrain,xTrain)
str(trainingData)

# Read in the test data
subjectTest = read.table('./test/subject_test.txt',header=FALSE); #imports subject_test.txt
xTest       = read.table('./test/x_test.txt',header=FALSE); #imports x_test.txt
yTest       = read.table('./test/y_test.txt',header=FALSE); #imports y_test.txt

# Assign column names to the test data imported above
colnames(subjectTest) = "subjectId"
colnames(xTest)       = features[,2]
colnames(yTest)       = "activityId"


# Create the final test set by merging the xTest, yTest and subjectTest data
testData <- cbind(yTest,subjectTest,xTest)
str(trainingData)
str(testData)

# Combine training and test data to create a final data set
finalData <- rbind(trainingData,testData)

# Create a vector for the column names from the finalData for mean and std variable.

colNames  <- colnames(finalData)
mean_std <- (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames))
finalData <- finalData[,mean_std]

# 3. Use descriptive activity names to name the activities in the data set
# Merge the finalData set with the acitivityType table to include descriptive activity names
finalData <- merge(finalData,activityType,by='activityId',all.x=TRUE);

# 4. Appropriately label the data set with descriptive activity names. 
colNames  = colnames(finalData)
for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};

colnames(finalData) = colNames

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

# delete the activityType column
finalData_by_ActivityType  <- finalData[,names(finalData) != 'activityType'];
str(finalData_by_ActivityType)

# Summarizing the finalDataNoActivityType table to include just the mean of each variable for each activity and each subject

tidyData    <- aggregate(finalData_by_ActivityType[,names(finalData_by_ActivityType) != c('activityId','subjectId')],by=list(activityId=finalData_by_ActivityType$activityId,subjectId = finalData_by_ActivityType$subjectId),mean)

# Merging the tidyData with activityType to include descriptive acitvity names
tidyData    <-  merge(tidyData,activityType,by='activityId',all.x=TRUE);

# Export the tidyData set 
write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t')
