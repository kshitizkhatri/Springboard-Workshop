# 11th January 2016

# Objective 1 of this R script is to merge the training and test sets to create one data set

# Objective 2 of this R script is to extract columns containing mean and standard deviation for each measurement

# Objective 3 of this script is to create variables called ActivityLabel and ActivityName that label all observations with the corresponding activity labels and names respectively

# Objective 4 of this script is to make a tidy data set with the average of each variable for each activity and each subject.

# I will start with importing all the data first. For making the process easier, one can set the working directory as per convinience for importing the data as per following code

features <- read.table (file.choose ())
subject_test <- read.table (file.choose ())
subject_train <- read.table (file.choose ())
x_train <- read.table (file.choose ())
Y_train<- read.table(file.choose())

X_test <- read.table (file.choose ())
Y_test <- read.table (file.choose ())

body_acc_X_train <- read.table (file.choose ())
body_acc_Y_train <- read.table (file.choose ())
body_acc_Z_train <- read.table (file.choose ())

body_gyro_X_train <- read.table (file.choose ())
body_gyro_Y_train <- read.table (file.choose ())
body_gyro_Z_train <- read.table (file.choose ())

total_acc_X_train <- read.table (file.choose ())
total_acc_Y_train <- read.table (file.choose ())
total_acc_Z_train <- read.table (file.choose ())

body_acc_X_test <- read.table (file.choose ())
body_acc_Y_test <- read.table (file.choose ())
body_acc_Z_test <- read.table (file.choose ())

body_gyro_X_test <- read.table (file.choose ())
body_gyro_Y_test <- read.table (file.choose ())
body_gyro_Z_test <- read.table (file.choose ())

total_acc_X_test <- read.table (file.choose ())
total_acc_Y_test <- read.table (file.choose ())
total_acc_Z_test <- read.table (file.choose ())

# We are done with our first objective here.

# Next step would be to merge the data frames to make a single data set.

# I am going to use merge () function for merging the x_test and X_train datasets

x_train_test_merged <- merge (X_test, x_train, all = TRUE)

# Now we can name the columns using the features data set with the help of colnames function

colnames (x_train_test_merged) <- features$V2

# We will use bind_rows () for merging Y_test and Y_train as both the data frames have numeric type data.

Y_train_test_merged <- bind_rows (Y_test, Y_train)

# Now merging both these variables to get one merged data frame.

consolidated_x_y <- data.frame (x_train_test_merged,Y_train_test_merged)

subject_total <- bind_rows (subject_test, subject_train)

consolidated_x_y <- data.frame (subject_total, consolidated_x_y)

colnames (consolidated_x_y) [1] <- "Subjects"

# WE are done with our objective 1

# Following code is to achieve objective 2
# I have used grep function to extract the text with the strings "mean" and "std"

std_consolidated <- grep ("std", colnames(consolidated_x_y))

mean_consolidated <- grep ("mean", colnames (consolidated_x_y))

consolidated_mean_std <- data.frame (consolidated_x_y [,std_consolidated], consolidated_x_y [, mean_consolidated])

# We are done with our objective 2

# Next step would be to define ActivityLabels and ActivityNames so as to label and name the observations

ActivityLAbel <- Activity_Label$V1 [consolidated_x_y$V1]

ActivityName <- Activity_Label$V2 [consolidated_x_y$V1]

# Functions mentioned above will generate two vectrors, first one a numeric vector containing 
# all the labels and second one a factor vector with all the names of the activities

# With this piece of code we are done with the third objective 

# To achieve the final objective, we will need tidyr library. Install that first

install.packages ("tidyr")

# Now we will use aggregate function for finding out the average of each variable for each activity and each subject

df1 <- aggregate (consolidated_x_y, by= list (Subjects, V1.1), FUN = mean)

gathered_final <- df1  %>% tidyr::gather(Subjects, V1.1, 4:564)

# WE will now rename the columns for gathered_final variable

colnames (gathered.final)[6] <- "Mean_Observations"

colnames (gathered_final)[5] <- "Variables"

# We are now done with all the objectives of producing this script. we will export it to a data table for submission.

write.table (gathered_final, "C:/Users/Kshitiz Khatri/Desktop/Springboard/getdata-projectfiles-UCI HAR Dataset/tidy.txt", sep="\t")

# Thank You







