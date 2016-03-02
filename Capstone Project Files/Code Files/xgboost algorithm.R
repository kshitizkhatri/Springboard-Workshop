## xgboost algorithm after feature engineering

library(lubridate)
library(xgboost)
library(caret)

setwd ("c:/Users/Kshitiz Khatri/Desktop/Springboard/AirBnb Kaggle Dataset/")

age_groups <- matrix(0, nrow = 21, ncol=2)
age_groups[,1] <- seq(0, 100, 5)
age_groups[,2] <- seq(4, 104, 5)
age_groups[21,2] <- 150
age_groups <- data.frame(age_groups)
names(age_groups) <- c("Lower", "Upper")

getCharRep <- function(ii) {
  paste(age_groups[ii,1], "-", age_groups[ii,2], sep = "")
}
age_groups$CharRep <- sapply(1:nrow(age_groups), getCharRep)
age_groups$CharRep[21] <- "100+"

test_usr_ids <- test_usrs$id
test_usrs$country_destination <- "NDF"

all_usrs <- rbind(train_usrs, test_usrs)
all_usrs <- all_usrs[, c(1, 16, 2:15)]

all_usrs$date_first_booking <- NULL

all_usrs$date_account_created <- ymd(all_usrs$date_account_created)
all_usrs$ACYear <- year(all_usrs$date_account_created)
all_usrs$ACMonth <- month(all_usrs$date_account_created)
all_usrs$ACDay <- day(all_usrs$date_account_created)
all_usrs$date_account_created <- NULL

all_usrs$timestamp_first_active <- as.character(all_usrs$timestamp_first_active)
all_usrs$FAYear <- as.integer(substring(all_usrs$timestamp_first_active, 1, 4))
all_usrs$FAMonth <- as.integer(substring(all_usrs$timestamp_first_active, 5, 6))
all_usrs$FADay <- as.integer(substring(all_usrs$timestamp_first_active, 7, 8))
all_usrs$timestamp_first_active <- NULL

all_usrs$age[is.na(all_usrs$age)] <- -1
all_usrs$age[all_usrs$age > 1900] <- 2015 - all_usrs$age[all_usrs$age > 1900]
all_usrs$age[all_usrs$age > 100] <- 100
all_usrs$age[(all_usrs$age < 14) & (all_usrs$age >= 0)] <- 14

ageGrpIdx <- ceiling((all_usrs$age + 1)/5)
ageGrpIdx[ageGrpIdx > 21] <- 21
all_usrs$AgeGrp <- "UNKN"
all_usrs$AgeGrp[ageGrpIdx > 0] <- age_groups$CharRep[ageGrpIdx]

all_usrs$AgeGrp <- factor(all_usrs$AgeGrp, levels = sort(unique(all_usrs$AgeGrp)))

all_usrs$gender <- as.factor(all_usrs$gender)

all_usrs$signup_method <- as.factor(all_usrs$signup_method)
all_usrs$signup_flow <- as.factor(all_usrs$signup_flow)
all_usrs$language <- as.factor(all_usrs$language)
all_usrs$affiliate_channel <- as.factor(all_usrs$affiliate_channel)
all_usrs$affiliate_provider <- as.factor(all_usrs$affiliate_provider)
all_usrs$first_affiliate_tracked <- as.factor(all_usrs$first_affiliate_tracked)

all_usrs$signup_app <- as.factor(all_usrs$signup_app)
all_usrs$first_device_type <- as.factor(all_usrs$first_device_type)
all_usrs$first_browser <- as.factor(all_usrs$first_browser)

## sorted in decreasing levels is prevelance
countryLevels <- c("NDF","US","other","FR","IT","GB","ES","CA","DE","NL","AU","PT")

all_usrs <- cbind(all_usrs[,c(1,2), drop=FALSE], model.matrix(~ -1 + ., all_usrs[,-c(1,2)]))
print(dim(all_usrs))

all_usrs$age[all_usrs$age == -1] <- NA

session_info <- read.csv ("sessions2.csv/sessions2.csv", stringsAsFactors = FALSE)

colsForBooking <- c("actionapply_coupon_click_success", "actionapply_reservation", "actionbooking",  
                    "actionchange_availability", "actionchange_currency", "actioncoupon_code_click", 
                    "actionpay", "actionpayment_methods", "actionprint_confirmation", "actionrate", 
                    "actionreceipt", "actionrecent_reservations", "action_detailapply_coupon", 
                    "action_detailapply_coupon_click", "action_detailapply_coupon_click_success",
                    "action_detailapply_coupon_error", "action_detailbooking", "action_detailbook_it",
                    "action_detailchange_availability", "action_detailchange_or_alter", 
                    "action_detailcreate_payment_instrument", "action_detailmodify_reservations")

colForNonEnglish <- c("actionajax_google_translate", "actionajax_google_translate_description", 
                      "actionajax_google_translate_reviews","actionchange_currency",
                      "actioncountry_options", "actionsouth.america", "actionsouthern.europe", 
                      "actionspoken_languages", "action_detailtranslate_listing_reviews", 
                      "action_detailtranslations","actionlanguages_multiselect","actionspoken_languages",
                      "action_detailuser_languages")

session_info$BookingDone <- rowSums(session_info[, colsForBooking], na.rm = TRUE)
session_info$OtherThanEnglish <- rowSums(session_info[, colForNonEnglish], na.rm = TRUE)

all_usrs <- merge(all_usrs, session_info, all.x = TRUE, by.x = "id", by.y = "user_id")

nonZeros <- function(xx) {
  xx[is.na(xx)] <- 0
  xx[xx > 1] <- 1
  return (sum(xx))
}

countInstances <- sapply(all_usrs[,-c(1,2)], nonZeros)
colToRemove <- which(countInstances <= 10)
length(colToRemove)
colToRemove <- colToRemove + 2 ### we had removed first two cols from summation, hence the vector which is sstarting from 1, will start with 3rd position, leaving the user ID and country_destination column in the matrix
all_usrs <- all_usrs[,-colToRemove]
dim(all_usrs)

all_usrs[is.na(all_usrs)] <- -99999

test_idx <- which(all_usrs$id %in% test_usr_ids)
test <- all_usrs[test_idx,]
test$country_destination <- NULL
train <- all_usrs[-test_idx,]

train_save <- train
test_save <- test

## do binary classification to create layers for each country

getBinaryOutcome <- function(train, test, countryName) {
  numFolds <- 2
  numRepeats <- 2
  numRounds <- numFolds*numRepeats
  
  label.country <- rep(0, nrow(train))
  label.country[train$country_destination == countryName] <- 1
  folds <- createMultiFolds(label.country, k = numFolds, times = numRepeats)
  
  train.pred <- rep(0, nrow(train))
  test.pred <- rep(0, nrow(test))
  
  test.DM  <- xgb.DMatrix(data = data.matrix(test[, -1]), missing = NA)
  
  for (ii in seq(1, length(folds)) ) {
    other.ids <- setdiff(1:nrow(train), folds[[ii]])
    print(length(other.ids))
    print(names(folds)[ii])
    
    train.DM  <- xgb.DMatrix(data = data.matrix(train[-other.ids,-c(1,2)]), 
                             label = label.country[-other.ids], missing = NA)
    other.DM  <- xgb.DMatrix(data = data.matrix(train[other.ids,-c(1,2)]), 
                             label = label.country[other.ids], missing = NA)
    
    wlist <- list(val = other.DM, train = train.DM)
    param <- list( max.depth = 6, eta = 0.03, booster = "gbtree",
                   subsample = 0.7, colsample_bytree = 0.7,
                   objective = "binary:logistic")#, eval_metric = "mlogloss") ## mlogloss, merror
    
    set.seed(6745)
    model1 <- xgb.train(params = param, data = train.DM, nrounds = 2000,
                        early.stop.round = 50,
                        nthread = 4, verbose = 1, print.every.n = 20,
                        missing = NA, watchlist = wlist, maximize = FALSE)
    
    bestIter <- model1$bestInd
    train.pred[other.ids] <- train.pred[other.ids] + predict(model1, newdata = other.DM, ntreelimit = bestIter)
    test.pred <- test.pred + predict(model1, newdata = test.DM, ntreelimit = bestIter)
  }
  
  test.pred <- test.pred/numRounds
  train.pred <- train.pred/numRepeats
  
  #train.pred <- ifelse(train.pred < 0.5, 0, 1)
  #test.pred <- ifelse(test.pred < 0.5, 0, 1)
  
  retval = list(trainpred = train.pred, testpred = test.pred)
  return (retval)
}

##   NDF     US  other     FR     IT     GB     ES     CA     DE     NL     AU     PT 
# 124543  62376  10094   5023   2835   2324   2249   1428   1061    762    539    217 

NDFlist <- getBinaryOutcome(train, test, "NDF")
USlist <- getBinaryOutcome(train, test, "US")
Otherlist <- getBinaryOutcome(train, test, "other")
FRlist <- getBinaryOutcome(train, test, "FR")
ITlist <- getBinaryOutcome(train, test, "IT")
GBlist <- getBinaryOutcome(train, test, "GB")
ESlist <- getBinaryOutcome(train, test, "ES")
CAlist <- getBinaryOutcome(train, test, "CA")
DElist <- getBinaryOutcome(train, test, "DE")
NLlist <- getBinaryOutcome(train, test, "NL")
AUlist <- getBinaryOutcome(train, test, "AU")
PTlist <- getBinaryOutcome(train, test, "PT")

train$PredNDF <- NDFlist$trainpred
test$PredNDF <- NDFlist$testpred

train$PredUS <- USlist$trainpred
test$PredUS <- USlist$testpred

train$PredOther <- Otherlist$trainpred
test$PredOther <- Otherlist$testpred

train$PredFR <- FRlist$trainpred
test$PredFR <- FRlist$testpred

train$PredIT <- ITlist$trainpred
test$PredIT <- ITlist$testpred

train$PredGB <- GBlist$trainpred
test$PredGB <- GBlist$testpred

train$PredES <- ESlist$trainpred
test$PredES <- ESlist$testpred

train$PredCA <- CAlist$trainpred
test$PredCA <- CAlist$testpred

train$PredDE <- DElist$trainpred
test$PredDE <- DElist$testpred

train$PredNL <- NLlist$trainpred
test$PredNL <- NLlist$testpred

train$PredAU <- AUlist$trainpred
test$PredAU <- AUlist$testpred

train$PredPT <- PTlist$trainpred
test$PredPT <- PTlist$testpred

#################################################

train <- train[sample(nrow(train)),]
test <- test[order(test$id),]

## Without validation set and without the binary outcome features

label.country <- as.integer(factor(train_save$country_destination, levels = countryLevels))
label.country <- label.country - 1
numClasses <- max(label.country) + 1


train_save_DM <- xgb.DMatrix(data = data.matrix (train_save[,-c (1,2)]), missing = NA, label = label.country)
test_save_DM <- xgb.DMatrix(data = data.matrix (test_save[,-c (1)]), missing = NA)
bst <- xgboost(data = train_save_DM, label = label.country, max.depth = 9, eta = 0.3, nthread = 2, nrounds = 200, objective = "multi:softmax", num_class = 12)
watchlist <- list (train = train_save_DM)
parameters <- list (max.depth = 6, eta = 0.1, nthread = 2, booster = "gbtree", num_class = numClasses, objective = "multi:softmax")
bst <- xgb.train (params = parameters, data = train_save_DM, watchlist = watchlist, label = label.country, nrounds = 400, early.stop.round = 20, verbose = 1, maximize = TRUE)
pred <- predict (bst, test_save_DM)
table (pred)

## With different parameter
bst <- xgb.train (params = parameters, data = train_save_DM, watchlist = watchlist, label = label.country, nrounds = 400, verbose = 1, maximize = TRUE)
pred <- predict (bst, test_save_DM)
table (pred)

## Change of parameters
parameters <- list (max.depth = 6, eta = 0.2, nthread = 2, booster = "gbtree", num_class = numClasses, objective = "multi:softmax")
bst <- xgb.train (params = parameters, data = train_save_DM, watchlist = watchlist, label = label.country, nrounds = 100, verbose = 1, maximize = TRUE)
pred <- predict (bst, test_save_DM)
table (pred)


## With validation and training set
library (caTools)
split_train_save <- sample.split(train_save$country_destination, SplitRatio = 0.7)
train_save_train <- subset (train_save, split_train_save == "TRUE")
train_save_val <- subset (train_save, split_train_save == "FALSE")


label.country.train <- as.integer(factor(train_save_train$country_destination, levels = countryLevels))
label.country.train <- label.country.train - 1
numClasses <- max(label.country.train) + 1

label.country.val <- as.integer(factor(train_save_val$country_destination, levels = countryLevels))
label.country.val <- label.country.val - 1
numClasses_val <- max(label.country.val) + 1


train_save_train_DM <- xgb.DMatrix(data = data.matrix (train_save[,-c (1,2)]), missing = NA, label = label.country)
train_save_val_DM <- xgb.DMatrix(data = data.matrix (train_save_val[,-c (1,2)]), missing = NA, label = label.country.val)

watchlist <- list (train = train_save_train_DM, test = train_save_val_DM)

bst <- xgboost(data = train_save_DM, label = label.country, max.depth = 9, eta = 0.3, nthread = 2, nrounds = 200, objective = "multi:softmax", num_class = 12)

parameters <- list (max.depth = 10, eta = 0.25, nthread = 4, booster = "gbtree", num_class = numClasses, objective = "multi:softmax")
bst1 <- xgb.train (params = parameters, data = train_save_train_DM, watchlist = watchlist, label = label.country.train, nrounds = 100, verbose = 1, maximize = TRUE)
pred <- predict (bst1, test_save_DM)
table (pred)

## Checking the results for a particular iteration
pred <- predict (bst1, test_save_DM, ntreelimit = 20)
table (pred)

## Kaggle submission file
pred_1 <- predict (bst1, test_save_DM)
test_usrs$country <- pred1
test_usrs$country <- test_usrs$country + 1
test_usrs$country <- factor (test_usrs$country, levels = c (1,2,3,4,5,6,7,8,9,10,11,12), labels = c ('NDF', 'US', 'other','FR','IT', 'GB', 'ES', 'CA', 'DE', 'NL', 'AU', 'PT'))
airbnb_kaggle_submission9 <- subset (test_usrs, select = c (1,17))
write.csv (airbnb_kaggle_submission9, "airbnb_kaggle_submission9.csv", quote = FALSE, row.names = FALSE)

## Changing parameter
parameters <- list (max.depth = 10, eta = 0.25, nthread = 4, booster = "gbtree", num_class = numClasses, objective = "multi:softmax", colsample_bytree = 0.6)
bst2 <- xgb.train (params = parameters, data = train_save_train_DM, watchlist = watchlist, label = label.country.train, nrounds = 100, verbose = 1, maximize = FALSE, early.stop.round = 20)
pred_2 <- predict (bst2, test_save_DM)

## Kaggle submission file
test_usrs$country <- pred_2
test_usrs$country <- test_usrs$country + 1
test_usrs$country <- factor (test_usrs$country, levels = c (1,2,3,4,5,6,7,8,9,10,11,12), labels = c ('NDF', 'US', 'other','FR','IT', 'GB', 'ES', 'CA', 'DE', 'NL', 'AU', 'PT'))
airbnb_kaggle_submission10 <- subset (test_usrs, select = c (1,17))
write.csv (airbnb_kaggle_submission10, "airbnb_kaggle_submission10.csv", quote = FALSE, row.names = FALSE)

## Changing parameters
parameters <- list (max.depth = 9, eta = 0.25, nthread = 4, booster = "gbtree", num_class = numClasses, objective = "multi:softmax", colsample_bytree = 0.7)
bst2 <- xgb.train (params = parameters, data = train_save_train_DM, watchlist = watchlist, label = label.country.train, nrounds = 200, verbose = 1, maximize = FALSE, early.stop.round = 20)
pred_3 <- predict (bst2, test_save_DM)
table (pred_3)

## Changing parameters
parameters <- list (max.depth = 9, eta = 0.22, nthread = 4, booster = "gbtree", num_class = numClasses, objective = "multi:softmax", colsample_bytree = 0.6, subsample = 0.6)
bst3 <- xgb.train (params = parameters, data = train_save_train_DM, watchlist = watchlist, label = label.country.train, nrounds = 200, verbose = 1, maximize = FALSE, early.stop.round = 20)
## While the meeror in the above model was high, hence I didn't test it on the test set.


## Training and validating set with the binary variables made by predicting binary outcome for each country.#####################################################################################
split_train_latest <- sample.split(train$country_destination, SplitRatio = 0.7)
train_training <- subset (train, split_train_latest == "TRUE")
train_validating <- subset (train, split_train_latest == "FALSE")

label.country.training <- as.integer(factor(train_training$country_destination, levels = countryLevels))
label.country.training <- label.country.training - 1
numClasses <- max(label.country.training) + 1

label.country.validating <- as.integer(factor(train_validating$country_destination, levels = countryLevels))
label.country.validating <- label.country.validating - 1

train_training_DM <- xgb.DMatrix(data = data.matrix (train_training[,-c (1,2)]), missing = NA, label = label.country.training)
train_validating_DM <- xgb.DMatrix(data = data.matrix (train_validating[,-c (1,2)]), missing = NA, label = label.country.validating)
watchlist <- list (train = train_training_DM, test = train_validating_DM)
parameters <- list (max.depth = 9, eta = 0.3, nthread = 4, booster = "gbtree", num_class = numClasses, objective = "multi:softmax", colsample_bytree = 0.6, subsample = 0.6)
bst4 <- xgb.train (params = parameters, data = train_training_DM, watchlist = watchlist, label = label.country.training, nrounds = 200, verbose = 1, maximize = FALSE, early.stop.round = 20)

bst5 <- xgb.train (params = parameters, data = train_training_DM, watchlist = watchlist, label = label.country.training, nrounds = 200, verbose = 1, maximize = TRUE, early.stop.round = 20, eval_metric = 'auc')

## Changing parameters
parameters <- list (max.depth = 9, eta = 0.22, nthread = 4, booster = "gbtree", num_class = numClasses, objective = "multi:softmax", colsample_bytree = 0.6, subsample = 0.6)
bst6 <- xgb.train (params = parameters, data = train_training_DM, watchlist = watchlist, label = label.country.training, nrounds = 200, verbose = 1, maximize = FALSE, early.stop.round = 20)

parameters <- list (max.depth = 9, eta = 0.22, nthread = 4, booster = "gbtree", num_class = numClasses, objective = "multi:softmax", colsample_bytree = 0.8, subsample = 0.8)
bst6 <- xgb.train (params = parameters, data = train_training_DM, watchlist = watchlist, label = label.country.training, nrounds = 200, verbose = 1, maximize = FALSE, early.stop.round = 20)

parameters <- list (max.depth = 6, eta = 0.2, nthread = 4, booster = "gbtree", num_class = numClasses, objective = "multi:softmax", colsample_bytree = 0.8, subsample = 0.5)
bst6 <- xgb.train (params = parameters, data = train_training_DM, watchlist = watchlist, label = label.country.training, nrounds = 100, verbose = 1, maximize = FALSE, early.stop.round = 20)

parameters <- list (max.depth = 9, eta = 0.2, nthread = 4, booster = "gbtree", num_class = numClasses, objective = "multi:softmax", colsample_bytree = 0.7)
bst7 <- xgb.train (params = parameters, data = train_training_DM, watchlist = watchlist, label = label.country.training, nrounds = 100, verbose = 1, maximize = FALSE, early.stop.round = 20)

parameters <- list (max.depth = 9, eta = 0.2, nthread = 4, booster = "gbtree", num_class = numClasses, objective = "multi:softmax", subsample = 0.8)
bst8 <- xgb.train (params = parameters, data = train_training_DM, watchlist = watchlist, label = label.country.training, nrounds = 100, verbose = 1, maximize = TRUE, early.stop.round = 20, eval_metric = 'auc')

## Preping the test data set for testing
testing_DM <- xgb.DMatrix(data = data.matrix (test[,-1]), missing = NA)

pred_5 <- predict(bst8, testing_DM)
table (pred_5)

pred_4 <- predict(bst7, testing_DM)
table (pred_4)
test_usrs$country <- pred_4
test_usrs$country <- test_usrs$country + 1
test_usrs$country <- factor (test_usrs$country, levels = c (1,2,3,4,5,6,7,8,9,10,11,12), labels = c ('NDF', 'US', 'other','FR','IT', 'GB', 'ES', 'CA', 'DE', 'NL', 'AU', 'PT'))
airbnb_kaggle_submission12 <- subset (test_usrs, select = c (1,17))
write.csv (airbnb_kaggle_submission12, "airbnb_kaggle_submission12.csv", quote = FALSE, row.names = FALSE)

pred_6 <- predict (bst6, testing_DM)
table (pred_6)

parameters <- list (max.depth = 9, eta = 0.03, nthread = 4, booster = "gbtree", num_class = numClasses, objective = "multi:softmax", subsample = 0.7)
bst9 <- xgb.train (params = parameters, data = train_training_DM, watchlist = watchlist, label = label.country.training, nrounds = 200, verbose = 1, maximize = FALSE, early.stop.round = 20)

pred_7 <- predict(bst9, testing_DM)
table (pred_7)

## I have tried as per the above code, there is more scope to improve the results by using hyper parameter optimization using various libraries in R.