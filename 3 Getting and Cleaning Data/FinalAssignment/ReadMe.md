```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Getting and Cleaning Data Course Project: Human Activity Recognition Using Smartphones Data Set

The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set.
The goal is to prepare tidy data that can be used for later analysis.

### Data Set

The raw data set can be downloaded from the following website: [<http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones#>]

Further information on how the data was read and manipulated can be found in the [CodeBook](CodeBook.Rmd).

### Files

-   CodeBook.Rmd: contains a description of the data, teh variables and any transformations or work performed to clean up the data.

-   run_analysis.R: a script that performs the following:

    -   Merges the training and the test sets to create one data set.

    -   Extracts only the measurements on the mean and standard deviation for each measurement.

    -   Uses descriptive activity names to name the activities in the data set.

    -   Appropriately labels the data set with descriptive variable names.

    -   From the data set in the previous step, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

-   Final_dataset.txt: tidy data set created in the last step of run_analysis.R
