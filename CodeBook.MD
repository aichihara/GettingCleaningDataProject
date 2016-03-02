CodeBook.MD

 

Getting and Cleaning Data Project 
Ana Travassos

Description

Code Book

The program:

1) Input Files:
•features.txt
•activity_labels.txt
•subject_train.txt
•x_train.txt
•y_train.txt
•subject_test.txt
•x_test.txt
•y_test.txt

2) Procedure:

2.a: Merge the training and the test sets to create one data set: Read in the train data and test data and make the merge using rbind.
2.b: Extract:  measurements,  mean and standard deviation for each measurement using a logical vector and subsets.
2.c: Features contains correct names. activityType  includes descriptive acitvity names.
2.d: Export results to a file tidyData.txt'
