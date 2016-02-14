library(dplyr)
library(data.table)

# Path to folder (which is work directory, as it wrote into requirements for the submission)
# where you unpuck zip - file with data
data_location <- getwd()


# Function for creating full paths to files with data
locate_files <- function(path) {
        paste(data_location, path, sep = "")
}

# Vector with paths of all required files into directory \getdata-projectfiles-UCI HAR Dataset,
# which is a root in zip - file with data
locations <- c(x_train = "\\getdata-projectfiles-UCI HAR Dataset\\train\\X_train.txt",
                 y_train = "\\getdata-projectfiles-UCI HAR Dataset\\train\\Y_train.txt",
                 subject_train = "\\getdata-projectfiles-UCI HAR Dataset\\train\\subject_train.txt",
                 x_test = "\\getdata-projectfiles-UCI HAR Dataset\\test\\X_test.txt",
                 y_test = "\\getdata-projectfiles-UCI HAR Dataset\\test\\Y_test.txt",
                 subject_test = "\\getdata-projectfiles-UCI HAR Dataset\\test\\subject_test.txt",
                 features = "\\getdata-projectfiles-UCI HAR Dataset\\features.txt",
                 labels = "\\getdata-projectfiles-UCI HAR Dataset\\activity_labels.txt")

# Create full paths to all files for next reading data from it.
paths <- lapply(locations, locate_files)



# This function merges X, Y, subject and activity labels data into one dataset,
# make selection of required features: subject, Activity and features with 
# data about only the measurements on the mean and standard deviation.
select_data <- function(x, y, subject, features, labels) {
        
        # Reading all separate dataframes with data about measurments
        # activities, subjects and activities labels
        data_x <- read.table(file = x)
        data_y <- read.table(file = y)
        data_features <- read.table(file = features, col.names = c("id", "name"))
        data_subject <- read.table(file = subject)
        data_labels <- read.table(file = labels, col.names = c("id", "Activity"))        
        
        # Set names 
        names <- paste(data_features$id, data_features$name, sep = "_")
        colnames(data_x) <- names
        colnames(data_y) <- "label"
        colnames(data_subject) <- "subject"

        # Select data about only the measurements on the mean and standard deviation
        data_x <- select(data_x, c(grep("mean()", names), grep("std()", names)))        
        
        # Set descriptive activity names to the activities in the data set
        data_y <- inner_join(data_y, data_labels, by = c("label" = "id"))
        data_y <- select(data_y, Activity)        
        
        # Mearge all data into one dataset
        cbind(data_subject, data_y, data_x)
}

# Merge all training data
train_data <- select_data(paths$x_train, paths$y_train, paths$subject_train, paths$features, paths$labels)
# Mearge all test data
test_data <- select_data(paths$x_test, paths$y_test, paths$subject_test, paths$features, paths$labels)
# Merge train and test data into dataset
data <- rbind(train_data, test_data)

# This function creates descriptive variable names for previously merged data
rename <- function(names) {
        names <- sub("*.{,100}_", "", names)
        names <- sub("^t+", "time", names)
        names <- sub("^f+", "freq", names)
        names <- sub("Acc+", "Accelerometer", names)
        names <- sub("Gyro+", "Gyroscope", names)
        names <- gsub("-", ".", names)
        names <- gsub("\\()", "", names)
        names <- sub("std+", "StdDiv", names)
        names
}

names(data) <- rename(names(data))
data <- as.data.table(data)

# Creation independent data frame with the average of each variable for 
# each activity and each subject.
final_data <- data[, lapply(.SD, mean), by = c("subject", "Activity")]

# Function for reset features names to be more discriptive
edit_names <- function(x){
        if(x %in% c("subject", "Activity")) {
                res <- x
                        
        } else {
            res <- paste("Average.of.", x, sep ="")        
        }
        res
}

names(final_data) <- sapply(names(final_data), edit_names)

# Writing final dataset into txt - file
write.table(x = final_data, file = 'run_analysis.txt', row.name = FALSE)