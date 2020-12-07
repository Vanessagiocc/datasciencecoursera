
library(dplyr)
fileurl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileurl,destfile="./week4.zip")
unzip("week4.zip")

#First read the required documents into a data.frame
activity_labels<-read.table("UCI HAR Dataset/activity_labels.txt",na.strings=" ")
features<-read.table("UCI HAR Dataset/features.txt",na.strings=" ")
subject_test<-read.table("UCI HAR Dataset/test/subject_test.txt",na.strings=" ")
x_test<-read.table("UCI HAR Dataset/test/X_test.txt",na.strings=" ")
y_test<-read.table("UCI HAR Dataset/test/y_test.txt",na.strings = " ")
subject_train<-read.table("UCI HAR Dataset/train/subject_train.txt", na.string=" ")
x_train<-read.table("UCI HAR Dataset/train/X_train.txt",na.strings=" ")
y_train<-read.table("UCI HAR Dataset/train/y_train.txt",na.strings=" ")

#Part of question 4: Rename variable names in test and train data.frame. 
names(subject_test)<-"Subject"
names(subject_train)<-"Subject"
names(y_test)<-"Activitys"
names(y_train)<-"Activitys"
names(x_test)<-features$V2
names(x_train)<-features$V2

#Add a column "Type" to know the dataset to which the measurement belongs
#Question 1:Merges the training and the test sets to create one data set

Type<-rep(c("Test"),times=2947)
Datatest=cbind(subject_test,y_test,Type,x_test)
Type<-rep(c("Train"),times=7352)
Datatrain=cbind(subject_train,y_train,Type,x_train)

Data_total<-rbind(Datatest,Datatrain)
#Question 2:Extracts only the measurements on the mean and standard deviation for each measurement.
Data_total<-Data_total%>%select(Subject,Activitys,Type,matches('mean|std'))

#Question 3:Uses descriptive activity names to name the activities in the data set
Data_total$Activitys<-sapply(Data_total$Activitys,function(x){x<-activity_labels[x,2]})
## Another way to do this:Data_total<-Data_total%>%mutate(activities=as.character(factor(activities,levels = 1:6,labels = activity_labels$V2)))#

#Question 4: remove some extra characters from variable names and change others.
names(Data_total)<-gsub("[-()]","",names(Data_total))
names(Data_total)<-gsub("mean","Mean",names(Data_total))
names(Data_total)<-gsub("std","Std",names(Data_total))

#Question 5:From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidy_data<-Data_total%>%group_by(Subject,Activitys)%>%summarise_at(vars(c(-Type)),mean)

write.table(tidy_data,"TidyData_CourseProject.txt",row.names=FALSE)