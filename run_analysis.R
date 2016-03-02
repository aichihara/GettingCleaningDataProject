########################################################################################################## 
## Coursera Getting and Cleaning Data Course Project 
## Ana Ichiara
## 29-02-2014
    
# runAnalysis.r File Description: 
    
# This script will perform the following steps on the UCI HAR Dataset downloaded from  
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip  
# 1. Merge the training and the test sets to create one data set. 
# 2. Extract only the measurements on the mean and standard deviation for each measurement.  
# 3. Use descriptive activity names to name the activities in the data set 
# 4. Appropriately label the data set with descriptive activity names.  
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.  
########################################################################################################## 

#########################################################################################
# 1.Initialization
#########################################################################################

# working directory
setwd('C:/hopkins/Projeto/UCI HAR Dataset/');

#########################################################################################
# 1.Merges the training and test to create one data set. 
#########################################################################################

# train data 
 x_train       = read.table('train/x_train.txt',header=FALSE); 
 y_train       = read.table('train/y_train.txt',header=FALSE); 
 features     = read.table('features.txt',header=FALSE); 
 activity_type = read.table('activity_labels.txt',header=FALSE); 
 subject_train = read.table('train/subject_train.txt',header=FALSE); 
 
 colnames(activity_type)  = c('activityId','activity_type'); 
 colnames(subject_train)  = "subjectId"; 
 colnames(x_train)        = features[,2];  
 colnames(y_train)        = "activityId"; 
 
 train_data = cbind(y_train,subject_train,x_train); 
  
 #  test data 
 subject_test = read.table('test/subject_test.txt',header=FALSE); 
 x_test       = read.table('test/x_test.txt',header=FALSE); 
 y_test       = read.table('test/y_test.txt',header=FALSE); 

 colnames(subject_test) = "subjectId"; 
 colnames(x_test)       = features[,2];  
 colnames(y_test)       = "activityId"; 
  
 test_data = cbind(y_test,subject_test,x_test); 
 
 #################  merges train and test data 
 
 res_data = rbind(train_data,test_data); 

##########################################################################################
2. Extracts only the measurements on the mean and standard deviation for each measurement. 
##########################################################################################
colNames  = colnames(res_data);
logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames)); 
res_data = res_data[logicalVector==TRUE]; 

########################################################################################
3. Uses descriptive activity names to name the activities in the data set 
#########################################################################################

res_data = merge(res_data,activity_type,by='activityId',all.x=TRUE);
colNames  = colnames(res_data); 

#########################################################################################
4. Appropriately labels the data set with descriptive variable names. 
#########################################################################################
for (k in 1:length(colNames))  
{ 
   colNames[k] = gsub("\\()","",colNames[k]) 
   colNames[k] = gsub("-std$","StdDev",colNames[k]) 
   colNames[k] = gsub("-mean","Mean",colNames[k]) 
   colNames[k] = gsub("^(t)","time",colNames[k]) 
   colNames[k] = gsub("^(f)","freq",colNames[k]) 
   colNames[k] = gsub("([Gg]ravity)","Gravity",colNames[k]) 
   colNames[k] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[k]) 
   colNames[k] = gsub("[Gg]yro","Gyro",colNames[k]) 
   colNames[k] = gsub("AccMag","AccMagnitude",colNames[k]) 
   colNames[k] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[k]) 
   colNames[k] = gsub("JerkMag","JerkMagnitude",colNames[k]) 
   colNames[k] = gsub("GyroMag","GyroMagnitude",colNames[k]) 
 }; 
 
 
colnames(res_data) = colNames; 

###################################################################################################################################################
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
################################################################################################################################################### 
 ResDataNoActivityType  = res_data[,names(res_data) != 'activityType']; 
 
 tidyData    = aggregate(ResDataNoActivityType[,names(ResDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=ResDataNoActivityType$activityId,subjectId = ResDataNoActivityType$subjectId),mean); 
 tidyData    = merge(tidyData,activityType,by='activityId',all.x=TRUE); 

 write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t'); 

