run_analysis <- function(data_wd = "./UCI HAR Dataset") {
     ## Arguments:
     ##   data_wd = directory/folder in which the data set exists
     ##   
     ## Purpose:
     ##   1. Merges the training and the test sets to create one data set.
     ##   2. Extracts only the measurements on the mean and standard deviation for each measurement.
     ##   3. Uses descriptive activity names to name the activities in the data set
     ##   4. Appropriately labels the data set with descriptive variable names.
     ##   5. From the data set in step 4, creates a second, independent tidy data set with the 
     ##      average of each variable for each activity and each subject.
     ##      

     ## Setting data_wd to ensure it is the full path
     start_wd <- getwd()
     setwd(data_wd)
     data_wd <- getwd()
     
     ## setwd(start_wd)
     
     ## load appropriate R packages
     library(dplyr)
     library(reshape2)
     
     ## Better label activity names in dataset
     ## 
     active_labels <- read.table("activity_labels.txt", header = FALSE)
     colnames(active_labels) <- c("active_key", "label")
     active_labels[,2] <- tolower(active_labels[,2])
     
     ## Improve variable names in dataset
     ## 
     feature_labels <- read.table("features.txt", header = FALSE)
     colnames(feature_labels) <- c("feature_key", "label")
     var_of_interest <- grep("mean|std", feature_labels[,2])
     
     ## Read training data
     ## Note first read all data then limit to just variables of interest as defined above
     ## 
     setwd("./train")

     ## Read data and limit to mean & standard dev
     temp_feature_data <- read.table("X_train.txt", header = FALSE)
     train_feature_data <- temp_feature_data[,var_of_interest]
     
     ## Add column names
     colnames(train_feature_data) <- feature_labels[var_of_interest,2]
     
     ## Read activity info, make factor with appropriate lables
     train_activity <- read.table("Y_train.txt", header = FALSE)
     train_act <- as.data.frame(factor(train_activity[,1],levels = active_labels[,1], 
                                      labels = active_labels[,2]))
     colnames(train_act) <- c("activity")
     
     ## Read in subject ID's
     train_subject <- read.table("subject_train.txt", colClasses = c("factor"), header = FALSE)
     colnames(train_subject) <- c("subject")

     ## Combine Data, activity, and subect into single table     
     train_tbl <- as.tbl(cbind(train_feature_data, train_act, train_subject))
     setwd(data_wd)
     
          
     ## Read test data
     ## Note first read all data then limit to just variables of interest as defined above
     ## 
     setwd("./test")
     ## Read data and limit to mean & standard dev
     temp_feature_data <- read.table("X_test.txt", header = FALSE)
     test_feature_data <- temp_feature_data[,var_of_interest]
     ## Add column names
     colnames(test_feature_data) <- feature_labels[var_of_interest,2]

     ## Read activity info, make factor with appropriate lables
     test_activity <- read.table("Y_test.txt", header = FALSE)
     test_act <- as.data.frame(factor(test_activity[,1],levels = active_labels[,1], 
                                      labels = active_labels[,2]))
     colnames(test_act) <- c("activity")
     
     ## Read in subject ID's
     test_subject <- read.table("subject_test.txt", colClasses = c("factor"), header = FALSE)
     colnames(test_subject) <- c("subject")

     ## Combine Data, activity, and subect into single table     
     test_tbl <- as.tbl(cbind(test_feature_data, test_act, test_subject))
     
     setwd(start_wd)

     ## Combine Train and Test data     
     allDataTbl <- bind_rows(test_tbl, train_tbl)
     allDataTbl$subject <- as.factor(allDataTbl$subject)
     allDataTbl <- group_by(allDataTbl, activity, subject)
     
     ## Summarize data. Generate mean for each variable grouped by: Activity & Subject
     ## 
     varName <- as.character(feature_labels[var_of_interest[1],2])
     summaryDataTbl <- summarize(allDataTbl, avg = mean(.data[[varName]]))
     groupNum <- length(groups(allDataTbl))
     colnames(summaryDataTbl)[groupNum + 1] <- paste(varName,"mean")

     for (i in 2:length(var_of_interest)) {
          varName <- as.character(feature_labels[var_of_interest[i],2])
          summaryDataTbl <- ungroup(summaryDataTbl)
          summaryDataTbl <- as.tbl(cbind(summaryDataTbl,
                                         summarize(allDataTbl, avg = mean(.data[[varName]]))[,groupNum + 1]))
          colnames(summaryDataTbl)[groupNum + i] <- paste(varName,"mean")
          
     }
     
     ## Write Summary table to file
     setwd(start_wd)
     write.table(summaryDataTbl, "Summary Data Table.txt", row.names = FALSE)
     
     summaryDataTbl
     
}