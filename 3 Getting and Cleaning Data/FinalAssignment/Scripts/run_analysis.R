# Load necessary package
library(dplyr)

# Merge the test and training sets to create one data set 
test <- cbind(subject_test, y_test, x_test)
train <- cbind(subject_train, y_train, x_train)
dataset <- rbind(test, train)
colnames(dataset) <- c("Subject", "Activity", features$Variables)

# Extract only the measurements on the mean and the standard deviation for each 
# measurement
dataset <- select(dataset, Subject, Activity, contains("mean()"), contains("std()"))

# Uses descriptive activity names to name the activities in the data set
dataset$Activity <- activities[dataset$Activity, 2]

# Appropriately labels the data set with descriptive variable names
variables <- colnames(dataset)
variables <- gsub("BodyBody", "Body", variables)
variables <- gsub("mean()", "Mean", variables)
variables <- gsub("std()", "STD", variables)
variables <- gsub("\\()", "", variables)
variables <- gsub("-", "", variables)
colnames(dataset) <- variables

# Create an independent tidy data set with the average of each variable for each
# activity and each variable
final_dataset <- group_by(dataset, Subject, Activity)
final_dataset <- summarise_all(final_dataset, .funs=mean)

write.table(final_dataset, file = "Output/final_dataset.txt", row.names = FALSE)
