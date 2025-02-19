Author: Alex Mazurkiewicz
Email: myemail@mail.com
Date created: 22 May 2016
Last revision: 22 May 2016

The R-script "run_analysis.R" calculates the mean of the mean and standard deviation parameters in the Samsung dataset.
This script reads in the TEST and TRAIN datasets from their respective folders, appending the respective subject numbers and activity types.
Next, only those features involving a calculated MEAN or STANDARD DEVIATION are extracted using the information provided in the features.txt file
(see the features_info.txt file for information on these features). Similarly, the activity numbers and their corresponding activity descriptions are
read in and appended to the TEST or TRAIN dataset.

The TEST and TRAIN datasets, which have different numbers of subjects (rows) but otherwise the same number of columns, are merged into one dataset
(all.data). The all.data dataset is then broken into groups by (i) Subject number and (ii) Activity type, and the arithmetic averages of the features
are calculated for each subject and activity. 

30 subjects x 6 activity types yields 180 rows in the final dataset, avg.data.

For more information regarding the data used in this analysis, please refer to the following sources:

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones (site where data was obtained -- descriptive)
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip (data used for this project)

Below, an excerpt from the original Readme file corresponding to the original data:

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 

For each record it is provided:
======================================

- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

The dataset includes the following files:
=========================================

- 'README.txt'

- 'features_info.txt': Shows information about the variables used on the feature vector.

- 'features.txt': List of all features.

- 'activity_labels.txt': Links the class labels with their activity name.

- 'train/X_train.txt': Training set.

- 'train/y_train.txt': Training labels.

- 'test/X_test.txt': Test set.

- 'test/y_test.txt': Test labels.

The following files are available for the train and test data. Their descriptions are equivalent. 

- 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 

- 'train/Inertial Signals/total_acc_x_train.txt': The acceleration signal from the smartphone accelerometer X axis in standard gravity units 'g'. Every row shows a 128 element vector. The same description applies for the 'total_acc_x_train.txt' and 'total_acc_z_train.txt' files for the Y and Z axis. 

- 'train/Inertial Signals/body_acc_x_train.txt': The body acceleration signal obtained by subtracting the gravity from the total acceleration. 

- 'train/Inertial Signals/body_gyro_x_train.txt': The angular velocity vector measured by the gyroscope for each window sample. The units are radians/second. 

Notes: 
======
- Features are normalized and bounded within [-1,1].
- Each feature vector is a row on the text file.

For more information about this dataset contact: activityrecognition@smartlab.ws


----------------------
Alex Mazurkiewicz
22.05.2016