# Read the training data
subjectTrainData <- read.table("UCI HAR Dataset/train/subject_train.txt", header=FALSE, sep="")
XTrainData <- read.table("UCI HAR Dataset/train/X_train.txt", header=FALSE, sep="")
yTrainData <- read.table("UCI HAR Dataset/train/y_train.txt", header=FALSE, sep="")
Train <- cbind.data.frame(subjectTrainData, XTrainData, yTrainData)

# Read the test data
subjectTestData <- read.table("UCI HAR Dataset/test/subject_test.txt", header=FALSE, sep="")
XTestData <- read.table("UCI HAR Dataset/test/X_test.txt", header=FALSE, sep="")
yTestData <- read.table("UCI HAR Dataset/test/y_test.txt", header=FALSE, sep="")
Test <- cbind.data.frame(subjectTestData, XTestData, yTestData)

# This is task 1: Merge the training and the test data sets to create one data set
myData <- rbind.data.frame(Train, Test)

# This is task 3: Read the features and activity labels and set the data as appropriate
activityLabels <- read.csv("UCI HAR Dataset/activity_labels.txt", header=FALSE, sep="")
features <- read.csv("UCI HAR Dataset/features.txt", header=FALSE, sep="")
featureNames <- c("subject", as.character(features[[2]]), "activity")
names(myData) <- featureNames

# This is task 2: Extract only the measurements on the mean and standard deviation for each measurement
myData[ ,563] <- factor(myData[ ,563], labels=as.character(activityLabels[[2]]))
neededVariables <- sort(c(1,grep("\\<mean()\\>",names(myData)),grep("\\<std()\\>",names(myData)),563))
myNewData <- myData[ ,neededVariables]

# This is task 4:  label the data set with descriptive variable or feature (column) names
myNewDataColNames <- gsub("[()]","",names(myNewData))
myNewDataColNames <- gsub("-","",myNewDataColNames)
myNewDataColNames <- gsub("mean","Mean",myNewDataColNames)
myNewDataColNames <- gsub("X",".X",myNewDataColNames)
myNewDataColNames <- gsub("Y",".Y",myNewDataColNames)
myNewDataColNames <- gsub("Z",".Z",myNewDataColNames)
myNewDataColNames <- gsub("std","Std",myNewDataColNames)

names(myNewData) <- myNewDataColNames

# Peek the data
head(myNewData)

# This is task 5: Create independent tidy data set with the average of each variable for each activity and each subject
tidyData <- aggregate(myNewData[,2:67],by=list(myNewData[,1], myNewData[,68]),mean)
names(tidyData) <- c("subject","activity",names(tidyData)[-c(1:2)])

# Peek tidy data set
head(tidyData)

# Write data to a text file
write.csv(tidyData, "tidyData.csv", quote=FALSE,row.names=FALSE)

########################################################################################
########################################################################################

# This is meant to generate a description of each variable, to be used in the code book
myNewDataVariableDescriptions <- NULL
for(name in names(tidyData)[-c(1,2)]){
     domain <- NULL
     lowPassFilter <- NULL
     rawSignal <- NULL
     signal <- NULL
     mag <- NULL
     estimate <- NULL
     direction <- NULL
     domain <- ifelse(grepl("^t",name),"Time Domain","Frequency Domain")
     lowPassFilter <- ifelse(grepl("Body",name),"Body","Gravity")
     rawSignal <- ifelse(grepl("Acc",name),"Acceleration","Gyroscope")
     signal <- ifelse(grepl("AccJerk",name),"Linear Acceleration",ifelse(grepl("GyroJerk",name),"Angular Velocity",""))
     mag <- ifelse(grepl("Mag",name),"Magnitude","")
     estimate <- ifelse(grepl("Mean",name),"Mean","Standard Deviation")
     direction <- ifelse(grepl(".X",name),"Direction X",ifelse(grepl(".Y",name),"Direction Y",ifelse(grepl(".Z",name),"Direction Z","")))
     temp <- paste(domain,lowPassFilter,rawSignal,signal,mag,estimate,direction,sep=" ")
     myNewDataVariableDescriptions <- c(myNewDataVariableDescriptions,temp)
}

# Final variable or column description object
VariableDescription <- c("Subject","Activity",myNewDataVariableDescriptions)
