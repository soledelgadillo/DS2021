## Project Description

The experiments have been carried out with a group of 30 volunteers
within an age bracket of 19-48 years. Each person performed six
activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING,
STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the
waist. Using its embedded accelerometer and gyroscope, both the 3-axial
linear acceleration and the 3-axial angular velocity at a constant rate
of 50Hz were captured. The experiments have been video-recorded to label
the data manually. The obtained dataset has been randomly partitioned
into two sets, where 70% of the volunteers was selected for generating
the training data and 30% the test data.

\#\#Study design and data processing

\#\#\#Collection of the raw data

The raw data set can be downloaded from the following website:
[<http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones#>]

```{r eval = FALSE}
# Download de file
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileURL, "Data/UCI HAR Data.zip")
unzip("Data/UCI HAR Data.zip")
# Save the download date
download_date <- Sys.time()
```

The script in this repository, first, reads the data from the files
listed below:

-   'features.txt': List of all features.
-   'activity_labels.txt': Links the class labels with their activity
    name.
-   'train/X_train.txt': Training set.
-   'train/y_train.txt': Training labels.
-   'test/X_test.txt': Test set.
-   'test/y_test.txt': Test labels.
-   'train/subject_train.txt' and 'test/subject_test.txt': Each row
    identifies the subject who performed the activity for each window
    sample. Its range is from 1 to 30.

For each record it is provided:

-   Triaxial acceleration from the accelerometer (total acceleration)
    and the estimated body acceleration.
-   Triaxial Angular velocity from the gyroscope.
-   A 561-feature vector with time and frequency domain variables.
-   Its activity label.
-   An identifier of the subject who carried out the experiment.
   
```{r eval = FALSE}
features <- read.table("Data/UCI HAR Dataset/features.txt", col.names = c("N", "Variables"))
activities <- read.table("Data/UCI HAR Dataset/activity_labels.txt", col.names = c("ActivitiesId", "Label"))
subject_test <- read.table("Data/UCI HAR Dataset/test/subject_test.txt", col.names = c("SubjectId"))
x_test <- read.table("Data/UCI HAR Dataset/test/X_test.txt", col.names = features$Variables)
y_test <- read.table("Data/UCI HAR Dataset/test/y_test.txt", col.names= "ActivitiesId")
subject_train <- read.table("Data/UCI HAR Dataset/train/subject_train.txt", col.names = c("SubjectId"))
x_train <- read.table("Data/UCI HAR Dataset/train/X_train.txt", col.names = features$Variables)
y_train <- read.table("Data/UCI HAR Dataset/train/y_train.txt", col.names = "ActivitiesId")
```

\#\#Creating the tidy datafile

The step by step is annotated in the
[run_analysis.R](Scripts/run_analysis.R) file.

\#\#Description of the variables in the final_dataset.txt file

In order to obtain this final data set, first, the script extracted only
the measurements on the mean and the standard deviation for each
measurement. With those variables selected, the final dataset was
created by calculating the average of each variable for each activity
and each subject. The final data set contains 180 observations across 68
variables.

```{r echo = TRUE}
dim(final_dataset)
```

### Variables

The first two variables in this data set are Subject and Activity:

1.  Subject (integer): identifies the subject who performed the activity
    for each window sample. Its range is from 1 to 30.
2.  Activity (character): each row identifies one of the six activities
    performed by the subjects during the experiment.

The rest of the variables are, actually, a selection of the features
selected for this data base and their values come from the accelerometer
and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ, respectively,
where the prefix 't' denotes time.

Also, the acceleration signal was separated into body and gravity
acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ). The body
acceleration signal was obtain by subtracting the gravity from the total
acceleration.

Subsequently, the body linear acceleration and angular velocity were
derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and
tBodyGyroJerk-XYZ).

Also the magnitude (Mag) of this three-dimensional signals were
calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag,
tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag).

Finally, a Fast Fourier Transform (FFT) was applied to some of these
signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ,
fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. Here the 't' was
replaced by 'f' to indicate frequency domain signals.

These signals were used to estimate variables of the feature vector for
each pattern: 'XYZ' is used to denote 3-axial signals in the X, Y and Z
directions.

Even though many variables were estimated from these signals, this
analysis only contains the mean and standard deviation of said signals.
Other variables, such as max() (largest value in the array) and min()
(smallest value in the array), among others, are present in the raw data
set.

The features have been normalized and bounded within [-1;1].

3.  tBodyAccMeanX: (numeric) Mean value of the body acceleration signal
    in X axis
4.  tBodyAccMeanY: (numeric) Mean value of the body acceleration signal
    in Y axis
5.  tBodyAccMeanZ: (numeric) Mean value of the body acceleration signal
    in Z axis
6.  tGravityAccMeanX: (numeric) Mean value of the gravity acceleration
    signal in X axis
7.  tGravityAccMeanY: (numeric) Mean value of the gravity acceleration
    signal in Y axis
8.  tGravityAccMeanZ: (numeric) Mean value of the gravity acceleration
    signal in Z axis
9.  tBodyAccJerkMeanX: (numeric) Mean value of the jerk derived from the
    body acceleration signal in X axis
10. tBodyAccJerkMeanY: (numeric) Mean value of the jerk derived from the
    body acceleration signal in Y axis
11. tBodyAccJerkMeanZ: (numeric) Mean value of the jerk derived from the
    body acceleration signal in Z axis
12. tBodyGyroMeanX: (numeric) Mean value of the angular velocity in X
    axis
13. tBodyGyroMeanY: (numeric) Mean value of the angular velocity in Y
    axis
14. tBodyGyroMeanZ: (numeric) Mean value of the angular velocity in Z
    axis
15. tBodyGyroJerkMeanX: (numeric) Mean value of the jerk derived from
    the angular velocity in X axis
16. tBodyGyroJerkMeanY: (numeric) Mean value of the jerk derived from
    the angular velocity in Y axis
17. tBodyGyroJerkMeanX: (numeric) Mean value of the jerk derived from
    the angular velocity in Z axis
18. tBodyAccMagMean: (numeric) Mean value of the magnitude of the
    triaxial body acceleration signal
19. tGravityAccMagMean: (numeric) Mean value of the magnitude of the
    gravity acceleration signal
20. tBodyAccJerkMagMean: (numeric) Mean value of the magnitude of the
    jerk derived from the body acceleration signal
21. tBodyGyroMagMean: (numeric) Mean value of the magnitude of the
    angular velocity (rad/s)
22. tBodyGyroJerkMagMean: (numeric) Mean value of the magnitude of the
    jerk derived from the angular velocity
23. fBodyAccMeanX: (numeric) Mean value of the Fast Fourier Transform of
    the body acceleration signal in X axis
24. fBodyAccMeanY: (numeric) Mean value of the Fast Fourier Transform of
    the body acceleration signal in Y axis
25. fBodyAccMeanZ: (numeric) Mean value of the Fast Fourier Transform of
    the body acceleration signal in Z axis
26. fBodyAccJerkMeanX: (numeric) Mean value of the Fast Fourier
    Transform of the jerk derived from the body acceleration signal in X
    axis
27. fBodyAccJerkMeanY: (numeric) Mean value of the Fast Fourier
    Transform of the jerk derived from the body acceleration signal in Y
    axis
28. fBodyAccJerkMeanZ: (numeric) Mean value of the Fast Fourier
    Transform of the jerk derived from the body acceleration signal in Z
    axis
29. fBodyGyroMeanX: (numeric) Mean value of the Fast Fourier Transform
    of the angular velocity signal in X axis
30. fBodyGyroMeanY: (numeric) Mean value of the Fast Fourier Transform
    of the angular velocity signal in Y axis
31. fBodyGyroMeanZ: (numeric) Mean value of the Fast Fourier Transform
    of the angular velocity signal in Z axis
32. fBodyAccMagMean: (numeric) Mean value of the Fast Fourier Transform
    of the magnitude of the body acceleration signal
33. fBodyAccJerkMagMean: (numeric) Mean value of the Fast Fourier
    Transform of the magnitude of the jerk derived from the body
    acceleration signal
34. fBodyGyroMagMean: (numeric) Mean value of the Fast Fourier Transform
    of the magnitude of the angular velocity signal
35. fBodyGyroJerkMagMean: (numeric) Mean value of the Fast Fourier
    Transform of the magnitude of the jerk derived from the angular
    velocity signal
36. tBodyAccSTDX: (numeric) Standard deviation value of the body
    acceleration signal in X axis
37. tBodyAccSTDY: (numeric) Standard deviation value of the body
    acceleration signal in Y axis
38. tBodyAccSTDZ: (numeric) Standard deviation value of the body
    acceleration signal in Z axis
39. tGravityAccSTDX: (numeric) Standard deviation value of the gravity
    acceleration signal in X axis
40. tGravityAccSTDY: (numeric) Standard deviation value of the gravity
    acceleration signal in Y axis
41. tGravityAccSTDZ: (numeric) Standard deviation value of the gravity
    acceleration signal in Z axis
42. tBodyAccJerkSTDX: (numeric) Standard deviation value of the jerk
    derived from the body acceleration signal in X axis
43. tBodyAccJerkSTDY: (numeric) Standard deviation value of the jerk
    derived from the body acceleration signal in Y axis
44. tBodyAccJerkSTDY: (numeric) Standard deviation value of the jerk
    derived from the body acceleration signal in Y axis
45. tBodyGyroSTDX: (numeric) Standard deviation value of the angular
    velocity signal in X axis
46. tBodyGyroSTDY: (numeric) Standard deviation value of the angular
    velocity signal in Y axis
47. tBodyGyroSTDZ: (numeric) Standard deviation value of the angular
    velocity signal in Z axis
48. tBodyGyroJerkSTDX: (numeric) Standard deviation value of the jerk
    derived from the angular velocity signal in X axis
49. tBodyGyroJerkSTDY: (numeric) Standard deviation value of the jerk
    derived from the angular velocity signal in Y axis
50. tBodyGyroJerkSTDZ: (numeric) Standard deviation value of the jerk
    derived from the angular velocity signal in Z axis
51. tBodyAccMagSTD: (numeric) Standard deviation value of the magnitude
    of the body acceleration signal
52. tGravityAccMagSTD: (numeric) Standard deviation value of the
    magnitude of the gravity acceleration signal
53. tBodyAccJerkMagSTD: (numeric) Standard deviation value of the
    magnitude of the jerk derived from the body acceleration signal
54. tBodyGyroMagSTD: (numeric) Standard deviation value of the magnitude
    of the angular velocity signal
55. tBodyGyroJerkMagSTD: (numeric) Standard deviation value of the
    magnitude of the jerk derived from the angular velocity signal
56. fBodyAccSTDX: (numeric): Standard deviation value of the Fast
    Fourier Transform of the body acceleration signal in X axis
57. fBodyAccSTDY: (numeric): Standard deviation value of the Fast
    Fourier Transform of the body acceleration signal in Y axis
58. fBodyAccSTDZ: (numeric): Standard deviation value of the Fast
    Fourier Transform of the body acceleration signal in Z axis
59. fBodyAccJerkSTDX: (numeric) Standard deviation value of the Fast
    Fourier Transform of the jerk derived from the body acceleration
    signal in X axis
60. fBodyAccJerkSTDY: (numeric) Standard deviation value of the Fast
    Fourier Transform of the jerk derived from the body acceleration
    signal in Y axis
61. fBodyAccJerkSTDZ: (numeric) Standard deviation value of the Fast
    Fourier Transform of the jerk derived from the body acceleration
    signal in Z axis
62. fBodyGyroSTDX: (numeric) Standard deviation value of the Fast
    Fourier Transform of the angular velocity in X axis
63. fBodyGyroSTDY: (numeric) Standard deviation value of the Fast
    Fourier Transform of the angular velocity in Y axis
64. fBodyGyroSTDZ: (numeric) Standard deviation value of the Fast
    Fourier Transform of the angular velocity in Z axis
65. fBodyAccMagSTD: (numeric) Standard deviation value of the Fast
    Fourier Transform of the body acceleration signal
66. fBodyAccJerkMagSTD: (numeric) Standard deviation value of the Fast
    Fourier Transform of the magnitude of the jerk derived from the body
    acceleration signal
67. fBodyGyroMagSTD: (numeric) Standard deviation value of the Fast
    Fourier Transform of the magnitude of the angular velocity signal
68. fBodyGyroJerkMagSTD: (numeric) Standard deviation value of the Fast
    Fourier Transform of the magnitude of the jerk derived from the
    angular velocity signal
