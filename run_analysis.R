library(plyr)
result_folder <- "Project"

##Reads and creates a complete data set
getData <- function(type, features){
        
        print(paste("Getting data", type))
        
        x_data <- getTable(paste(type,"/","X_",type,".txt",sep=""),features$V2) 
        y_data <- getTable(paste(type,"/","y_",type,".txt",sep=""),"activity")    
        subject_data <- getTable(paste(type,"/","subject_",type,".txt",sep=""),"id")
        
        return (cbind(subject_data,y_data,x_data)) 
}

##saves the data into the Project folder
saveResult <- function (data,name){
        
        print(paste("Saving data", name))
        
        file <- paste(result_folder, "se/", name,".csv" ,sep="")
        write.csv(data,file)
}




#features used for col names when creating train and test data sets
features <- getTable("features.txt")

## 1. Merges the training and the test sets to create one data set.

train <- getData("train",features)
test <- getData("test",features)
data <- rbind(train, test)

# rearrange the data using id
data <- arrange(data, id)

## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
dataset1 <- data[,c(1,2,grep("std", colnames(data)), grep("mean", colnames(data)))]
# save dataset1 into results folder
saveResult(dataset1,"dataset1")

## 3. Uses descriptive activity names to name the activities in the data set 
activity_labels <- getTable("activity_labels.txt")

## 4. Appropriately labels the data set with descriptive activity names.  
data$activity <- factor(data$activity, levels=activity_labels$V1, labels=activity_labels$V2)

## 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
dataset2 <- ddply(dataset1, .(id, activity), .fun=function(x){ colMeans(x[,-c(1:2)]) })

# Adds "_mean" to colnames
colnames(dataset2)[-c(1:2)] <- paste(colnames(dataset2)[-c(1:2)], "_mean", sep="")

# Save tidy dataset2 into results folder
saveResult(dataset2,"dataset2")
