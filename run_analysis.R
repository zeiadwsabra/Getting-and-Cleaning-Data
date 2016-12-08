#readDataSet reads   either the traing or testing data 
## readDataSet(0) for reading the traing data sets
## readDataSet(1) for reading the testing data sets

readDataSet <- function(choose = 0){
        if (choose  == 0) {folder <- "train"}
        else {
                folder <- "test"
        }
        
        library(data.table)
        ## read feature names
        featuresPath <- paste0("UCI HAR Dataset//","features.txt")
        features <- fread(featuresPath)$V2
        
        ## read subject column 
        path <-  paste0("UCI HAR Dataset//",folder)
        subjectPath <- paste0(path, paste0('//subject_',folder,'.txt'))
        subject <- as.numeric(readLines(subjectPath))
        
        ## read data columns
        dataPath <-  paste0(path, paste0('//X_',folder,'.txt'))
        data <- fread(dataPath)

        ## read activity column
        activityPath <- paste0(path, paste0('//y_',folder,'.txt'))
        activity <- as.numeric(readLines(activityPath))
        
        ## read descriptive activity names
        activityDescrptPath <- paste0("UCI HAR Dataset//","activity_labels.txt")
        activityDescript <- fread(activityDescrptPath)$V2
        
        ##convert activity ids to descriptive names 
        mappedActivity <- sapply(activity, function(x) activityDescript[x])
        
        ## combine the columns togther into a data frame 
        data <- data.frame(cbind(subject,data, mappedActivity))
        colnames(data) <- c("subject", features,'activity')
        data
}

# combining the traing and testing data

fullData <- rbind(readDataSet(0),readDataSet(1))

# expression for matching columns containing with mean or std
reg_epression <- "(mean|std)"
selectedColsFlag <- grepl(reg_epression, colnames(fullData))

# including subject col and activity col to selectedColsFlag
selectedColsFlag[1] = TRUE
selectedColsFlag[length(selectedColsFlag)] = TRUE

# subsetting the Data
selectedCols <- colnames(fullData)[selectedColsFlag]
fullData <- fullData[,selectedCols]


#average of each variable for each activity and each subject.
tidyData <- aggregate(. ~ subject + activity, fullData,mean)
tidyData <- tidyData[order(tidyData$subject), ]

#write the tidy data to tidy.txt
write.table(tidyData,"tidy.txt",row.name=FALSE)

