## Capstone Project

## Reading the data

library (ggplot2)
library (dplyr)
library (tidyr)


setwd ("c:/Users/Kshitiz Khatri/Desktop/Springboard/AirBnb Kaggle Dataset/")

sessions <- read.csv ("age_gender_bkts.csv/age_gender_bkts.csv", header = TRUE, sep = ",")

age_gender_bkts <- read.csv ("age_gender_bkts.csv/age_gender_bkts.csv", header = TRUE, sep = ",")

countries <- read.csv ("countries.csv/countries.csv", header = TRUE, sep = ",")

train_users <- read.csv ("train_users_2.csv/train_users_2.csv", header = TRUE, sep = ",")

sessions <- read.csv ("sessions.csv/sessions.csv", header = TRUE, sep = ",")
## Since there are quite a lot NAs in the data set, I am thinking of omitting it.
## The sessions data set is quite large with more than 10 million observations, so looks like a logical step.

sessions_new <- na.omit (sessions)

sessions_grouped <- group_by (sessions_new, user_id)

session_users <- table (sessions_grouped$user_id)

time_elapsed_sum <- summarize (sessions_grouped, total_time  = sum (secs_elapsed), n = n ())

time_elapsed_mean <- summarize (sessions_grouped, total_time  = mean (secs_elapsed), n = n ())

View (train_users)

train_users <- read.csv ("train_users_2.csv/train_users_2.csv", header = TRUE, sep = ",", na.strings = c ("", NA))

## Next step I have taken is to identify whether if there is any common ID in the sessions and the training data set.
subset (sessions_grouped, user_id == "osr2jwljor")
subset (sessions_grouped, user_id == "7my0vrljxc")

## I randomly selected two different variables out of the table and searched it in the sessions data frame.
## Next step is to group the training data set and to derive some insights from it

## First to check whether the data has significant number of NAs in it
table (is.na (train_users$age))

## More than 40% age data is not specified or NA. But I will see for the rest and divide that into differenoups as present in the age buckets table.
train_without_NA <- na.omit (train_users)

age_gender_bkts <- subset (age_gender_bkts, age_bucket != "100+")

## Now ordering the data by age_bucket and then plotting the population against it to look for a trend.

age_gender_bkts <- age_gender_bkts [order (age_gender_bkts$age_bucket),]

age_gender_bkt_distribution <- ggplot (aes (x = age_bucket, y = population_in_thousands), data = age_gender_bkts) + geom_point ()
age_gender_bkt_distribution
ggsave ('PopulationVsAge_bucket.png')

## The graph plotted through the code just above gives a rough idea about how the population in thousands travelling through AirBnB is distributed as per age.
## Note - Here I have assumed that this population represents the number of people travelling through AirBnB, that is friends, family and aquaintances of the people with the account ID.

## From the graph generated previously, it can be seen that there are certain data points which are far up higher for every range of age values.
## Simply looking at the data can tell that these data points belong to the US.
## Next step would be to plot the same curve for population chosing US as the destination country.

age_gender_bkt_US <- group_by (age_gender_bkt_US, age_bucket, gender)

age_gender_bkt_distribution_US <- ggplot (aes (x = age_bucket, y = population_in_thousands), data = age_gender_bkt_US) + geom_point () + facet_wrap(~gender)
age_gender_bkt_distribution_US
ggsave ('US_populationVsAge_bucket.png')

## I will now try to narrow down the first curve to just the population except the US population, to check whether, the other population follows the same trend or not by limiting the y axis.

age_gender_bkt_dist_countrywise <- ggplot (aes (x = age_bucket, y = population_in_thousands), data = age_gender_bkts) + geom_point () + facet_wrap(~country_destination, scales = "free") + ylim (0,4000)
age_gender_bkt_dist_countrywise
ggsave ('Age_Gender_bkt_dist_countriwise.png')

## Following steps will give me the variable containing the summary for the population chunk visiting every destination country
age_gender_bkts_countrywise <- group_by (age_gender_bkts, country_destination)
destination_countrywise_pop_sum <- summarize (age_gender_bkts_countrywise, pop_sum = sum (population_in_thousands), n= n())
destination_countrywise_pop_sum$pop_ratio <- destination_countrywise_pop_sum$pop_sum/sum (destination_countrywise_pop_sum$pop_sum)
View (destination_countrywise_pop_sum)
## Now I am going to pick the train_users data set, firstly checking whether there are any empty values in the destination country column.
table (is.na (train_users$country_destination))

## Next step would be to convert all the categorical varibles into numeric so that it will be easy to run operations on these.

train_users$gendern <- as.numeric (train_users$gender)

train_users$country_destination_n <- as.numeric (train_users$country_destination)

train_users$date_accCreated_formatted <- as.Date(train_users$date_account_created, "%Y-%m-%d")
library (lubridate)
train_users$date_accCreated_day <- day(train_users$date_accCreated_formatted)
train_users$date_accCreated_month <- month(train_users$date_accCreated_formatted)
train_users$date_timestamp <- as.Date(ymd_hms(train_users$timestamp_first_active))
train_users$timestamp_accountCreated_interval <- (train_users$date_timestamp - train_users$date_accCreated_formatted)

## Ploting a histogram for the count of gendern variable.
## count turns out to be highest for unspecified or 'unknown' gender, then females, males and lastly others.
qplot (x = gendern, data = train_users)
ggsave ('gender_by_count_hist.png')

## Plotting the gender count again, but this time with the color distinction of destination country
gender_count_destinationWise <- ggplot (aes (x = gendern, fill = country_destination), data = train_users) + geom_histogram()
gender_count_destinationWise
ggsave ('gender_count_destinationWise.png')

## I will now switch back to the training data set and try and find out the relation between different categorical variables with the destination countries variable
new_train_users <- subset (train_users, select = -c (5))
new_train_users$signup_method_n <- as.numeric(new_train_users$signup_method)
new_train_users$signup_language_n <- as.numeric(new_train_users$language)
new_train_users$affiliate_channel_n <- as.numeric(new_train_users$affiliate_channel)
new_train_users$affiliate_provider_n <- as.numeric(new_train_users$affiliate_provider)
new_train_users$first_affiliate_tracked_n <- as.numeric(new_train_users$first_affiliate_tracked)
new_train_users$signup_app_n <- as.numeric(new_train_users$signup_app)
new_train_users$first_device_type_n <- as.numeric(new_train_users$first_device_type)
new_train_users$first_browser_n <- as.numeric(new_train_users$first_browser)

## Using the variables created above, I will now define the matrices for each categorical variable and destination country, to ultimatlely check the correlation between each such pair.
matrix_gender <- as.matrix (table (new_train_users$country_destination, new_train_users$gendern))
matrix_signup_method <- as.matrix (table (new_train_users$country_destination, new_train_users$signup_method))
matrix_signup_flow <- as.matrix (table (new_train_users$country_destination, new_train_users$signup_flow))
matrix_first_affiliate_tracked <- as.matrix (table (new_train_users$country_destination, new_train_users$first_affiliate_tracked))
matrix_affiliate_provider <- as.matrix (table (new_train_users$country_destination, new_train_users$affiliate_provider))
matrix_affiliate_channel <- as.matrix (table (new_train_users$country_destination, new_train_users$affiliate_channel))
matrix_language <- as.matrix (table (new_train_users$country_destination, new_train_users$language))
matrix_signup_app <- as.matrix (table (new_train_users$country_destination, new_train_users$signup_app))
matrix_first_device_type <- as.matrix (table (new_train_users$country_destination, new_train_users$first_device_type))
matrix_first_browser <- as.matrix (table (new_train_users$country_destination, new_train_users$first_browser))

## Now I will take each matrix one at a time and try to find the correlation between each pair.

chisq.test(matrix_gender, simulate.p.value = TRUE)
chisq.test(matrix_signup_method, simulate.p.value = TRUE)
chisq.test(matrix_signup_flow, simulate.p.value = TRUE)
chisq.test(matrix_first_affiliate_tracked, simulate.p.value = TRUE)
chisq.test(matrix_affiliate_provider, simulate.p.value = TRUE)
chisq.test(matrix_affiliate_channel, simulate.p.value = TRUE)
chisq.test(matrix_language, simulate.p.value = TRUE)
chisq.test(matrix_signup_app, simulate.p.value = TRUE)
chisq.test(matrix_first_device_type, simulate.p.value = TRUE)
chisq.test(matrix_first_browser, simulate.p.value = TRUE)

## For the modeling purpose, I will now make a new data frame with only the categorical variables converted to numeric and the response variable as it is. I have removed dates as well just to keep the data set simple.

temp_train_users <- subset (new_train_users, select = -c (2,3,4,6,8:14,17), (age< 95 & age > 17)| is.na (age) )

## Now I will load the mlogit package for making a model to predict the destination country in the training data set
install.packages("mlogit")
library (mlogit)
library (randomForest)
library (xgboost)
library (mice)

## Since age is an important variable as observed through the analysis of the age_gender_bkts earlier, I cannot remove the missing value rows for age as that might affect the quality of analysis, so I will impute the age variable to fill up the missing values.
## For the ease of imputation, I just have kept 9 variables in the new data.

temp_train_users1 <- subset (temp_train_users, select = -c (1,2,9,10,11))

temp_train_users1_imputed <- complete (mice (temp_train_users1))

## Replacing the age from imputed data frame to the temp_train_users data set
temp_train_users$age <- temp_train_users1_imputed$age

## Now I will run the random forest formula to model for country_destination as the response variable with 5 different variables chosen by me. I tried to run it with more variables but my system is not supporting it further I have reduced the ntree to 400 (default is 500) so as it can be supported by my system.
model2 <- randomForest(country_destination ~ age + gendern + signup_language_n + affiliate_provider_n + first_browser_n,data = temp_train_users, ntree = 400)

## For testing the model, I need to make some alterations to the test data set as well, below is the code for it.

test_users_temp <- subset (test_users, select = -c (2,3,4))
test_users_temp$gendern <- as.numeric (test_users_temp$gender)
test_users_temp$signup_language_n <- as.numeric (test_users_temp$language)
test_users_temp$affiliate_provider_n <- as.numeric (test_users_temp$affiliate_provider)
test_users_temp$first_browser_n <- as.numeric (test_users_temp$first_browser)

## Testing the model on the modified test data set
Predicted_result <- predict(model2,test_users_temp )

## It turns out that test data set also had a lots of NA values in the age variable, I will now impute the age variable in the test set as well and then use the model on the new data set.
test_users_temp$signup_flow_n <- as.numeric (test_users_temp$signup_flow)
test_users_temp$signup_method_n <- as.numeric (test_users_temp$signup_method)
test_users_temp$signup_app_n <- as.numeric (test_users_temp$signup_app)
test_users_temp$first_device_type_n <- as.numeric (test_users_temp$first_device_type)
test_users_temp$affiliate_channel_n <- as.numeric (test_users_temp$affiliate_channel)
test_users_temp1 <- subset (test_users_temp, select = -c (1, 3:11))

test_users_temp1_imputed <- complete (mice(test_users_temp1,m = 2))

test_users_temp$age <- test_users_temp1_imputed$age

model2 <- randomForest(country_destination ~ age + gendern + signup_language_n + affiliate_provider_n + first_browser_n,data = temp_train_users, ntree = 400)

Predicted_result1 <- predict(model2,test_users_temp )

test_users$country <- Predicted_result1

airbnb_kaggle_submission1 <- subset (test_users, select = c (1,16))

## This step is to write the file for submission so as to evaluate the accuracy of the model
write.csv (airbnb_kaggle_submission1, "airbnb_kaggle_submission2.csv", quote = FALSE, row.names = FALSE)

## Moving further and trying different models with parameter tuning
model2 <- randomForest(country_destination ~ age + gendern + affiliate_provider_n + first_browser_n,data = temp_train_users, nodesize = 25,ntree = 400)

Predicted_result2 <- predict(model2,test_users_temp )
test_users$country <- Predicted_result2
airbnb_kaggle_submission1 <- subset (test_users, select = c (1,16))
write.csv (airbnb_kaggle_submission1, "airbnb_kaggle_submission2.csv", quote = FALSE, row.names = FALSE)

model3 <- randomForest(country_destination ~ age + gendern + affiliate_provider_n + first_browser_n + signup_language_n,data = temp_train_users, nodesize = 25,ntree = 400)
model4 <- randomForest(country_destination ~ age + gendern + affiliate_provider_n + first_browser_n + signup_language_n + signup_flow ,data = temp_train_users, nodesize = 25,ntree = 400)
Predicted_result3 <- predict(model4,test_users_temp )
test_users$country <- Predicted_result3
airbnb_kaggle_submission3 <- subset (test_users, select = c (1,16))
write.csv (airbnb_kaggle_submission3, "airbnb_kaggle_submission3.csv", quote = FALSE, row.names = FALSE)

model5 <- randomForest(country_destination ~ age + gendern + affiliate_provider_n + first_browser_n + signup_flow ,data = temp_train_users, nodesize = 25,ntree = 400)
Predicted_result4 <- predict(model5,test_users_temp )
test_users$country <- Predicted_result4
airbnb_kaggle_submission4 <- subset (test_users, select = c (1,16))
write.csv (airbnb_kaggle_submission4, "airbnb_kaggle_submission4.csv", quote = FALSE, row.names = FALSE)

model6 <- randomForest(country_destination ~ age + gendern + affiliate_channel_n + first_browser_n + signup_language_n ,data = temp_train_users, nodesize = 25,ntree = 400)
Predicted_result5 <- predict(model6,test_users_temp )
test_users$country <- Predicted_result5
airbnb_kaggle_submission5 <- subset (test_users, select = c (1,16))
write.csv (airbnb_kaggle_submission5, "airbnb_kaggle_submission5.csv", quote = FALSE, row.names = FALSE)

model7 <- randomForest(country_destination ~ age + gendern + first_affiliate_tracked_n + first_browser_n + signup_language_n ,data = temp_train_users, nodesize = 25,ntree = 400)
temp_train_users_without_na <- na.omit(temp_train_users)
model7 <- randomForest(country_destination ~ age + gendern + first_affiliate_tracked_n + first_browser_n + signup_language_n ,data = temp_train_users_without_na, nodesize = 25,ntree = 400)
Predicted_result6 <- predict(model7,test_users_temp )
test_users_temp$first_affiliate_tracked_n <- as.numeric(test_users_temp$first_affiliate_tracked)
Predicted_result6 <- predict(model7,test_users_temp )
test_users$country <- Predicted_result6
airbnb_kaggle_submission6 <- subset (test_users, select = c (1,16))
write.csv (airbnb_kaggle_submission6, "airbnb_kaggle_submission6.csv", quote = FALSE, row.names = FALSE)

## All the models constructed above were for the Kaggle submissions, now I will split the training data into two more data sets, training subset and test subset so that I can use model validation techniques on the data
library (caTools)
split <- sample.split(temp_train_users_without_na$country_destination,SplitRatio = 0.7)
train_airbnb <- subset (temp_train_users_without_na, split == TRUE)
test_airbnb <- subset (temp_train_users_without_na, split == FALSE)

model_airbnb_1 <- randomForest(country_destination ~ age + gendern + first_affiliate_tracked_n + first_browser_n + signup_language_n + first_device_type_n + signup_app_n ,data = train_airbnb, nodesize = 25,ntree = 200)
Predicted_airbnb1 <- predict(model_airbnb_1,test_airbnb )
matrix_airbnb_1 <- as.matrix (table (test_airbnb$country_destination, Predicted_airbnb1))
## I have manuaaly calculated the overall accuracy of the model, since I couldn't find any easier way to do it.
sum (32766,2,4181)/sum (matrix_airbnb_1)

model_airbnb_2 <- randomForest(country_destination ~ age + gendern + first_affiliate_tracked_n + first_browser_n + signup_language_n + first_device_type_n + signup_app_n ,data = train_airbnb, nodesize = 25,ntree = 400)
Predicted_airbnb2 <- predict(model_airbnb_2,test_airbnb )
matrix_airbnb_2 <- as.matrix (table (test_airbnb$country_destination, Predicted_airbnb2))
matrix_airbnb_2
sum (32794,6,4186)/sum (matrix_airbnb_2)

model_airbnb_3 <- randomForest(country_destination ~ age + gendern + first_affiliate_tracked_n + first_browser_n + signup_language_n ,data = train_airbnb, nodesize = 25,ntree = 400)
Predicted_airbnb3 <- predict(model_airbnb_3,test_airbnb )
matrix_airbnb_3 <- as.matrix (table (test_airbnb$country_destination, Predicted_airbnb3))
matrix_airbnb_3
sum (32468,20,4668)/sum (matrix_airbnb_3)

## I have calculated the accuracy of the model and individually for each category using above model, which turned out the best accuracy as per Kaggle.
## Next step would be to try and balance the data used for modeling as the result is just interpreting three categories due to a major imbalancing of the data.

install.packages("DMwR")
library (DMwR)
model_airbnb_smoted <- SMOTE (country_destination ~ age + gendern + first_affiliate_tracked_n + first_browser_n + signup_language_n ,data = train_airbnb,perc.over = 200, k = 5, perc.under = 200, learner = 'randomForest', nodesize = 25, ntree = 300 )
Predicted_airbnb_smoted <- predict(model_airbnb_smoted,test_airbnb )
matrix_airbnb_smoted <- as.matrix (table (test_airbnb$country_destination, Predicted_airbnb_smoted))
matrix_airbnb_smoted
## Above matrix shows that predictions are still pretty skewed, only instead of others variable, it is predicting PT with very high number of false negatives

model_airbnb_smoted <- SMOTE (country_destination ~ age + gendern + first_affiliate_tracked_n + first_browser_n + signup_language_n ,data = train_airbnb,perc.over = 2100, k = 5)
model_airbnb_smoted1 <- randomForest(country_destination ~ age + gendern + first_affiliate_tracked_n + first_browser_n + signup_language_n ,data = model_airbnb_smoted, nodesize = 25,ntree = 200)
Predicted_airbnb_smoted <- predict(model_airbnb_smoted1,test_airbnb )
matrix_airbnb_smoted <- as.matrix (table (test_airbnb$country_destination, Predicted_airbnb_smoted))
matrix_airbnb_smoted
## This matrix also shows the predicted values to be highly skewed towards couple of variables

model_airbnb_smoted <- SMOTE (country_destination ~ age + gendern + first_affiliate_tracked_n + first_browser_n + signup_language_n ,data = train_airbnb,perc.under = 2100, k = 11)
model_airbnb_smoted1 <- randomForest(country_destination ~ age + gendern + first_affiliate_tracked_n + first_browser_n + signup_language_n ,data = model_airbnb_smoted, nodesize = 25,ntree = 200)
Predicted_airbnb_smoted <- predict(model_airbnb_smoted1,test_airbnb )
matrix_airbnb_smoted <- as.matrix (table (test_airbnb$country_destination, Predicted_airbnb_smoted))
matrix_airbnb_smoted
## Tried with a small tweak, still producinghighly skewed results

install.packages("rpart")
library (rpart)
model_airbnb_rpart1 <- rpart (country_destination ~ age + gendern + first_affiliate_tracked_n + first_browser_n + signup_language_n ,data = train_airbnb, parms = list (prior = c (0.5,0.5)))
## Above command just throws an error of wrong length of priors.

model_airbnb_rpart1 <- rpart (country_destination ~ age + gendern + first_affiliate_tracked_n + first_browser_n + signup_language_n ,data = train_airbnb, parms = list (prior = c (0.6,0.4)))
## Tried above command, still throws the same error.

install.packages("ROSE")
library (ROSE)

train_airbnb$country_destination_binary <- ifelse(train_airbnb$country_destination == "NDF", "NDF", "NonNDF")
train_airbnb_rose <- ROSE(country_destination_binary ~ age + gendern + first_affiliate_tracked_n + first_browser_n + signup_language_n ,data = train_airbnb,p= 0.4)
View (train_airbnb_rose$data)
## The ROSE package is converting my dataframe containing various categorical variables to continuous variables with decimal values, I am not sure whether I should move ahead with this model or not.

## I again started to work on the SMOTE function and the data set to try different values to undersample and oversample

model_airbnb_smoted <- SMOTE (country_destination ~ age + gendern + first_affiliate_tracked_n + first_browser_n + signup_language_n ,data = train_airbnb,perc.under = 2100, k = 11, perc.over = 200)
train_airbnb$country_destination_binary <- NULL
model_airbnb_smoted <- SMOTE (country_destination ~ age + gendern + first_affiliate_tracked_n + first_browser_n + signup_language_n ,data = train_airbnb,perc.under = 200, k = 11, perc.over = 200)
model_airbnb_smoted <- SMOTE (country_destination ~ age + gendern + first_affiliate_tracked_n + first_browser_n + signup_language_n ,data = train_airbnb,perc.under = 2100, k = 11, perc.over = 400)
table (model_airbnb_smoted$country_destination)

model_airbnb_smoted <- SMOTE (country_destination ~ age + gendern + first_affiliate_tracked_n + first_browser_n + signup_language_n ,data = train_airbnb,perc.under = 2100, k = 11, perc.over = 1000)
table (model_airbnb_smoted$country_destination)

model_airbnb_smoted <- SMOTE (country_destination ~ age + gendern + first_affiliate_tracked_n + first_browser_n + signup_language_n ,data = train_airbnb,perc.under = 3000, k = 11, perc.over = 400)
table (model_airbnb_smoted$country_destination)

## I was running table function after each step so as to get a rough idea about what proportion of sampling is getting done for the response variable, country_destination
## I tried to model for a particular set of output produced after smotting to check if there is any improvement in the results of the output.

model_airbnb_smoted <- SMOTE (country_destination ~ age + gendern + first_affiliate_tracked_n + first_browser_n + signup_language_n ,data = train_airbnb,perc.under = 2100, k = 11, perc.over = 400)
model_airbnb_smoted1 <- randomForest(country_destination ~ age + gendern + first_affiliate_tracked_n + first_browser_n + signup_language_n ,data = model_airbnb_smoted, nodesize = 25,ntree = 300)
Predicted_airbnb_smoted <- predict(model_airbnb_smoted1,test_airbnb )
matrix_airbnb_smoted <- as.matrix (table (test_airbnb$country_destination, Predicted_airbnb_smoted))
matrix_airbnb_smoted
## The matrix produced from the code above again gave a very skewed results

## I will now take the original training set with imputed age values, and try to introduce the a new variable for country_destination which is in binary format and then split the data into NDF and non NDF.
temp_train_users_without_na$country_destination_binary <- ifelse (temp_train_users_without_na$country_destination == "NDF", "NDF", "NonNDF")

## I will create a a subset of the data set modified just above to use it for later purpose
temp_train_users_without_na_NonNDF <- droplevels (subset (temp_train_users_without_na, country_destination_binary == "NonNDF"))

## Back to the data set with binary country destination 
split_binary <- sample.split(temp_train_users_without_na$country_destination_binary,SplitRatio = 0.7)

train_airbnb_binary <- subset (temp_train_users_without_na, split_binary == TRUE)
test_airbnb_binary <- subset (temp_train_users_without_na, split_binary == FALSE)

## Balancing the binary training data set
## First I have used rpart extensively to find out the best performing length of priors to find the highest accuracy.

airbnb_rpart_binary <- rpart (country_destination_binary ~ age + gendern + first_affiliate_tracked_n + first_browser_n + signup_language_n ,data = train_airbnb_binary, parms = list (prior = c (0.6,0.4)), method = "class")
plotcp(airbnb_rpart_binary)
airbnb_rpart_binary1 <- prune(airbnb_rpart_binary, cp = 0.01)
Predict_airbnb_rpart <- predict(airbnb_rpart_binary1, test_airbnb_binary, type = "class")
matrix_airbnb_rpart <- as.matrix (table (test_airbnb_binary$country_destination_binary, Predict_airbnb_rpart))
matrix_airbnb_rpart
(26902+10899)/(26902+8801+14842+10899)

airbnb_rpart_binary <- rpart (country_destination_binary ~ age + gendern + first_affiliate_tracked_n + first_browser_n + signup_language_n ,data = train_airbnb_binary, parms = list (prior = c (0.5,0.5)), method = "class")
plotcp(airbnb_rpart_binary)
airbnb_rpart_binary1 <- prune(airbnb_rpart_binary, cp = 0.047)
Predict_airbnb_rpart <- predict(airbnb_rpart_binary1, test_airbnb_binary, type = "class")
matrix_airbnb_rpart <- as.matrix (table (test_airbnb_binary$country_destination_binary, Predict_airbnb_rpart))
matrix_airbnb_rpart
(19197+17521)/(19197+16506+8220+17521) ## This step will repeat for all the model, this is the manual calculation for the model accuracy as per the matrix printed after prediction of each model

airbnb_rpart_binary <- rpart (country_destination_binary ~ age + gendern + first_affiliate_tracked_n + first_browser_n + signup_language_n ,data = train_airbnb_binary, parms = list (prior = c (0.55,0.45)), method = "class")
plotcp(airbnb_rpart_binary)
airbnb_rpart_binary1 <- prune(airbnb_rpart_binary, cp = 0.034)
Predict_airbnb_rpart <- predict(airbnb_rpart_binary1, test_airbnb_binary, type = "class")
matrix_airbnb_rpart <- as.matrix (table (test_airbnb_binary$country_destination_binary, Predict_airbnb_rpart))
matrix_airbnb_rpart ## Manually checked for accuracy didn't turn out better than the previous ones

## I have tried few more combinations of the length of priors, the best result comes out to be for a length of 60:40.

## Now I will try the random forest classifier for the data without any balancing.
train_airbnb_binary$country_destination_binary <- as.factor (train_airbnb_binary$country_destination_binary)
airbnb_rf_binary <- randomForest (country_destination_binary ~ age + gendern + first_browser_n + signup_language_n ,data = train_airbnb_binary,nodesize = 25, ntree = 400)
Predict_airbnb_rpart <- predict(airbnb_rf_binary, test_airbnb_binary)
matrix_airbnb_rpart <- as.matrix (table (test_airbnb_binary$country_destination_binary, Predict_airbnb_rpart))
matrix_airbnb_rpart
(24785+16236)/(24785+10918+9505+16236)

## Same as last step but with one extra independent variable
airbnb_rf_binary <- randomForest (country_destination_binary ~ age + gendern + first_browser_n + signup_language_n + first_affiliate_tracked_n,data = train_airbnb_binary,nodesize = 25, ntree = 400)
Predict_airbnb_rpart <- predict(airbnb_rf_binary, test_airbnb_binary)
matrix_airbnb_rpart <- as.matrix (table (test_airbnb_binary$country_destination_binary, Predict_airbnb_rpart))
matrix_airbnb_rpart

## I will now try to balance using ROSE library and then apply random forest classification on the balanced data set
library (ROSE)
airbnb_rose_data <- ROSE(country_destination_binary ~ age + gendern + first_browser_n + signup_language_n ,data = train_airbnb_binary, N= 100000, p = 0.5)
airbnb_rf_binary_roseData <- randomForest (country_destination_binary ~ age + gendern + first_browser_n + signup_language_n ,data = airbnb_rose_data$data,nodesize = 25, ntree = 400)
Predicted_airbnb_rose <- predict (airbnb_rf_binary_roseData, test_airbnb_binary)
matrix_airbnb_rose <- as.matrix (table (test_airbnb_binary$country_destination_binary, Predicted_airbnb_rose))
matrix_airbnb_rose
(17596+19154)/(18107+17596+6587+19154)

airbnb_rose_data <- ROSE(country_destination_binary ~ age + gendern + first_browser_n + signup_language_n ,data = train_airbnb_binary, N= 100000, p = 0.3)
airbnb_rf_binary_roseData <- randomForest (country_destination_binary ~ age + gendern + first_browser_n + signup_language_n ,data = airbnb_rose_data$data,nodesize = 25, ntree = 400)
Predicted_airbnb_rose <- predict (airbnb_rf_binary_roseData, test_airbnb_binary)
matrix_airbnb_rose <- as.matrix (table (test_airbnb_binary$country_destination_binary, Predicted_airbnb_rose))
matrix_airbnb_rose

airbnb_rose_data <- ROSE(country_destination_binary ~ age + gendern + first_browser_n + signup_language_n ,data = train_airbnb_binary, N= 100000, p = 0.4)
airbnb_rf_binary_roseData <- randomForest (country_destination_binary ~ age + gendern + first_browser_n + signup_language_n ,data = airbnb_rose_data$data,nodesize = 25, ntree = 400)
Predicted_airbnb_rose <- predict (airbnb_rf_binary_roseData, test_airbnb_binary)
matrix_airbnb_rose <- as.matrix (table (test_airbnb_binary$country_destination_binary, Predicted_airbnb_rose))
matrix_airbnb_rose
(23217+14568)/(23217+12486+11173+14568)

## I will now work on the data which I had seperated earlier containing only the non NDF values.

library (caTools)

temp_train_users_without_na_NonNDF$cdUS <- ifelse (temp_train_users_without_na_NonNDF$country_destination == "US", "US", "NonUS")

split_binary_NonNDF <- sample.split(temp_train_users_without_na_NonNDF$cdUS, SplitRatio = 0.7)
train_NonNDf_split <- subset (temp_train_users_without_na_NonNDF, split_binary_NonNDF == "TRUE")
test_NonNDf_split <- subset (temp_train_users_without_na_NonNDF, split_binary_NonNDF == "FALSE")

## Since there is a imbalance in the data due to very high count for US, I am again going to sample the data and introduce a new column for US or NonUS for later use.

train_NonNDf_split$cdUS <- as.factor (train_NonNDf_split$cdUS)

model_NonNDF_US <- randomForest(cdUS~ age + gendern + first_browser_n + signup_language_n, data = train_NonNDf_split, nodesize = 25, ntree = 400)
Predicted_NonNDF_US <- predict (model_NonNDF_US, test_NonNDf_split)
matrix_NonNDF_US <- as.matrix (table (test_NonNDf_split$cdUS, Predicted_NonNDF_US))
matrix_NonNDF_US
(454+18447)/(454+7600+314+18447)

## I have got an accuracy of 70 % with just using random forest right away.
## I am building another model using ROSE package so that accuracy for Non US prediction might increase.

NonNDF_US_rose_data1 <- ROSE(cdUS~ age + gendern + first_browser_n + signup_language_n, data = train_NonNDf_split, N = 50000, p = 0.5)
model_NonNDF_US1 <- randomForest(cdUS~ age + gendern + first_browser_n + signup_language_n, data = NonNDF_US_rose_data1$data, nodesize = 25, ntree = 400)
Predicted_NonNDF_US1 <- predict (model_NonNDF_US1, test_NonNDf_split)
matrix_NonNDF_US1 <- as.matrix (table (test_NonNDf_split$cdUS, Predicted_NonNDF_US1))
matrix_NonNDF_US1
(16610+1448)/(6606+1448+16610+2151)

## Now I will further break down the data into other and NonOther category, and then apply suitable model, to predict the other vategory in the destination countries.

temp_train_users_without_na_NonNDF_NonUS <- droplevels (subset (temp_train_users_without_na_NonNDF, cdUS == "NonUS"))

temp_train_users_without_na_NonNDF_NonUS$cdOther <-  ifelse (temp_train_users_without_na_NonNDF_NonUS$country_destination == "other", "other", "NonOther")

split_NonNDF_NonUS_other <- sample.split (temp_train_users_without_na_NonNDF_NonUS, SplitRatio = 0.7)
train_NonNDF_NonUS_other <- subset (temp_train_users_without_na_NonNDF_NonUS, split_NonNDF_NonUS_other == "TRUE")
test_NonNDF_NonUS_other <- subset (temp_train_users_without_na_NonNDF_NonUS, split_NonNDF_NonUS_other == "FALSE")
temp_train_users_without_na_NonNDF_NonUS$cdOther <- as.factor (temp_train_users_without_na_NonNDF_NonUS$cdOther)

train_NonNDF_NonUS_other$cdOther <- as.factor (train_NonNDF_NonUS_other$cdOther)
test_NonNDF_NonUS_other$cdOther <- as.factor (test_NonNDF_NonUS_other$cdOther)

model_NonNDF_NonUS_other <- randomForest(cdOther~age + gendern + first_browser_n + signup_language_n, data = train_NonNDF_NonUS_other, nodesize = 25, ntree = 400)
Predict_NonNDF_NonUs_Other <- predict (model_NonNDF_NonUS_other, test_NonNDF_NonUS_other)
matrix_NonNDF_NonUS_other <- as.matrix (table (test_NonNDF_NonUS_other$cdOther, Predict_NonNDF_NonUs_Other))
matrix_NonNDF_NonUS_other

## Now the final task is to subset the data for NonOther category and then model for predicting the rest of the countries.

temp_train_users_without_na_NonNDF_NonUS_NonOther <- droplevels (subset (temp_train_users_without_na_NonNDF_NonUS, cdOther == "NonOther"))
split_NonOther <- sample.split(temp_train_users_without_na_NonNDF_NonUS_NonOther, SplitRatio = 0.7)
train_NonOther <- subset (temp_train_users_without_na_NonNDF_NonUS_NonOther, split_NonOther == "TRUE")
test_NonOther <- subset (temp_train_users_without_na_NonNDF_NonUS_NonOther, split_NonOther == "FALSE")
model_NonOther <- randomForest(country_destination ~ age + gendern + first_browser_n + signup_language_n, data = train_NonOther, nodesize = 25, ntree = 200)
Predited_NonOther <- predict (model_NonOther, test_NonOther)
matrix_NonOther <- as.matrix (table (test_NonOther$country_destination, Predited_NonOther))
matrix_NonOther


## Trying to model without age variable and with the month of the date when the account was created.
model_dates <- randomForest(country_destination_binary ~ age + signup_language_n + gendern + date_accCreated_month + first_browser_n,data = train_users_datesTrain, nodesize = 25,ntree = 400)
Predicted_date <- predict (model_dates, test_users_datestest)
matrix_dates <- as.matrix (table (test_users_datestest$country_destination_binary, Predicted_date))
matrix_dates

## Sessions data make sessions R script 
setwd("~/Documents/kaggle/airbnb")

library(lubridate)


sessions <- read.csv("sessions.csv/sessions.csv", stringsAsFactors = FALSE)

sessions$secs_elapsed[is.na(sessions$secs_elapsed)] <- 0
sessions$action[is.na(sessions$action)] <- "BLANK"
sessions$action_type[is.na(sessions$action_type)] <- "BLANK"
sessions$action_detail[is.na(sessions$action_detail)] <- "BLANK"

sessions$action[sessions$action == ""] <- "BLANK"
sessions$action_type[sessions$action_type == ""] <- "BLANK"
sessions$action_detail[sessions$action_detail == ""] <- "BLANK"

sessions$action <- factor(sessions$action, levels = sort(unique(sessions$action)))
sessions$action_type <- factor(sessions$action_type, levels = sort(unique(sessions$action_type)))
sessions$action_detail <- factor(sessions$action_detail, levels = sort(unique(sessions$action_detail)))
sessions$device_type <- factor(sessions$device_type, levels = sort(unique(sessions$device_type)))

sessions$count <- 1
sessions$secs_elapsed <- NULL

imax <- nrow(sessions) 
batchSize <- 100000
i1 <- 1
i2 <- batchSize

ss2 <- cbind(sessions[i1:i2,1, drop=FALSE], model.matrix(~ -1 + ., sessions[i1:i2,-1]))
ss2[, 2:(ncol(ss2)-1)] <- ss2[,2:(ncol(ss2)-1)] * ss2$count
sessions2 <- aggregate(.~user_id, data = ss2, FUN=sum)
print(paste("Done with ...", i1, "to", i2))
print(dim(sessions2))

while (i2 < imax) {  
  i1 <- i2+1
  i2 <- i1 + (batchSize-1)
  if (i2 > imax) i2 <- imax
  
  ss2 <- cbind(sessions[i1:i2, 1, drop=FALSE], model.matrix(~ -1 + ., sessions[i1:i2, -1]))
  ss2[, 2:(ncol(ss2)-1)] <- ss2[,2:(ncol(ss2)-1)] * ss2$count
  print(dim(ss2))
  sessions2 <- aggregate(.~user_id, data=rbind(sessions2, ss2), FUN=sum)
  print(dim(sessions2))
  
  print(paste("Done with ...", i1, "to", i2))
}

write.csv(sessions2, "./data/sessions2.csv", row.names = FALSE)

