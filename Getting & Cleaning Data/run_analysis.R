# run_analysis.R - Getting & Cleaning Data Coursera Project - HJM

# 0. LOAD DATA

# metadata
activity_labels<-read.table(file="~/Downloads/UCI HAR Dataset/activity_labels.txt",header=FALSE,stringsAsFactors=FALSE)
names(activity_labels)=c("actValue","actLabel")
features<-read.table("~/Downloads/UCI HAR Dataset/features.txt")
names(features)=c("colNo","featureName")

# train
XTrain<-read.table("~/Downloads/UCI HAR Dataset/train/X_train.txt",header=FALSE)
actTrain<-read.table("~/Downloads/UCI HAR Dataset/train/y_train.txt",header=FALSE)
subjectTrain<-read.table(file="~/Downloads/UCI HAR Dataset/train/subject_train.txt",header=FALSE)

# test
XTest<-read.table("~/Downloads/UCI HAR Dataset/test/X_test.txt",header=FALSE)
actTest<-read.table("~/Downloads/UCI HAR Dataset/test/y_test.txt",header=FALSE)
subjectTest<-read.table("~/Downloads/UCI HAR Dataset/test/subject_test.txt",header=FALSE)

# 1. MERGE TEST & TRAINING

# merge test & train
subject <-rbind(subjectTrain,subjectTest)
activity<-rbind(actTrain,actTest)
data<-rbind(XTrain,XTest)

# name columns
colnames(data)<-t(features[2])
colnames(activity)<- "Activity"
colnames(subject)<-"Subject"

# merge columns
df<- as.data.frame(cbind(data,activity,subject))

# clean up
rm(features,
   XTest,XTrain,
   actTest,actTrain,
   subjectTest,subjectTrain,
   subject,activity,data)

# 2. EXTRACT MEASUREMENTS ON MEAN & SD FOR EACH MEASUREMENT

measureCols<- grep(".*Mean.*|.*Std.*",names(df),ignore.case=TRUE)
measurements<- df[,c(measureCols,562,563)]

# 3. USE DESCRIPTIVE NAMES TO NAME ACTIVITIES IN THE DATASET

measurements<- mutate(measurements, Activity = activity_labels$actLabel[measurements$Activity])

# clean up
rm(measureCols,df,activity_labels)

# 4. APPROPRIATELY LABEL DATASET WITH DESCRIPTIVE VARIABLE NAMES

names(measurements)<-gsub("Acc", "Accelerometer", names(measurements))
names(measurements)<-gsub("Gyro", "Gyroscope", names(measurements))
names(measurements)<-gsub("BodyBody", "Body", names(measurements))
names(measurements)<-gsub("Mag", "Magnitude", names(measurements))
names(measurements)<-gsub("^f", "Frequency", names(measurements))
names(measurements)<-gsub("^t", "Time", names(measurements))
names(measurements)<-gsub("-mean", "Mean", names(measurements))
names(measurements)<-gsub("-std", "STD", names(measurements))
names(measurements)<-gsub("nFreq", "nFrequency", names(measurements))
names(measurements)<-gsub("tBody", "TimeBody", names(measurements))

# 5. CREATE 2ND TIDY DATASET WITH AVERAGE OF EACH VARIABLE FOR EACH ACTIVITY AND EACH SUBJECT

measurements$Subject<-as.factor(measurements$Subject)
measurements$Activity<-as.factor(measurements$Activity)

tidyMeasures<- arrange(aggregate(.~Subject + Activity,measurements,mean), Subject, Activity)

# SAVE 2ND TIDY DATASET AS .txt FILE

write.table(tidyMeasures,file="tidyMeasures.txt",row.names=FALSE)