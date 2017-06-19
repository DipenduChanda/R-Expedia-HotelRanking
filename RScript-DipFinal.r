#Expedia final project for BA with R
setwd("C:/Users/chand/Box Sync/Study/Sem 1/BA with Rattle/Final Project-Expedia/data")
getwd()

# Load library
library(randomForest)
library(rattle)
library(rpart)
library(rpart.plot)
library(rpart)
library(rpart.plot)
library(readxl)
library(rpart)
library(rpart.plot)
library(Hmisc)
library(rattle)
library(plyr)
library(caTools)
#For Imputing missing Values
library(mice)


#This command loads the expedia data set into a variable called "traindata."
traindata = read.csv("train.csv") 
str(traindata)


#This command writes the expedia data set for country 2 in a new csv file called "traincnty"
traincnty = subset(traindata, prop_country_id == 2)
nrow(traincnty)
write.csv(traincnty, "C:/Users/chand/Box Sync/Study/Sem 1/BA with Rattle/Final Project-Expedia/data/traincnty.csv")
summary(traincnty)

#Creating equally distributed subset with proper weightage 
grossdata = subset(traindata,gross_bookings_usd !="NULL")
str(gsRtGnull)
gsRtGnull = subset(traindata, gross_bookings_usd =="NULL")
gsRtGnull = subset(gsRtGnull, random_bool == "0")
eqdata= rbind(gsRtGnull[1:276592, ],  grossdata)
remove(grossdata,gsRtGnull)
str(eqdata)
# New dataframe with useful columns
eqdata2=eqdata[c("srch_id","date_time","site_id","visitor_location_country_id","prop_id","prop_starrating","prop_review_score","prop_brand_bool","prop_location_score1","prop_location_score2","prop_log_historical_price","position","price_usd","promotion_flag","srch_destination_id","srch_length_of_stay","srch_booking_window","srch_adults_count","srch_children_count","srch_room_count","srch_saturday_night_bool","random_bool","booking_bool","gross_bookings_usd","click_bool")] 
str(eqdata2)
#remove(eqdata)

#Changing the value from NULL to NA's
eqdata2$prop_location_score2[eqdata2$prop_location_score2 == "NULL"] <- NA
eqdata2$prop_review_score[eqdata2$prop_review_score == "NULL"] <- NA
eqdata2$gross_bookings_usd[eqdata2$gross_bookings_usd == "NULL"] <- NA

#Impute NA values of these 2 columns.
set.seed(88)
simple = eqdata2[c("prop_review_score","prop_starrating","price_usd","prop_location_score1","prop_location_score2","promotion_flag","srch_room_count")]
imputed = complete(mice(simple))
summary(imputed)
eqdata2$prop_location_score2 = imputed$prop_location_score2
eqdata2$prop_review_score = imputed$prop_review_score
summary(eqdata2)

# Code to remove these NA rows
eqdata2=eqdata2[!(is.na(eqdata2$prop_location_score2) | eqdata2$prop_location_score2==""), ]
eqdata2=eqdata2[!(is.na(eqdata2$prop_review_score) | eqdata2$prop_review_score==""), ]
str(eqdata2)
summary(eqdata2$prop_location_score2)
summary(eqdata2$prop_review_score)
eqdata2$prop_review_score <- as.factor(eqdata2$prop_review_score)
eqdata2$prop_starrating <- as.factor(eqdata2$prop_starrating)
eqdata2$prop_brand_bool <- as.factor(eqdata2$prop_brand_bool)
eqdata2$prop_location_score2 <- as.numeric(eqdata2$prop_location_score2)
eqdata2$promotion_flag <- as.factor(eqdata2$promotion_flag)
eqdata2$srch_length_of_stay <- as.factor(eqdata2$srch_length_of_stay)
eqdata2$srch_booking_window <- as.factor(eqdata2$srch_booking_window)
eqdata2$srch_adults_count <- as.factor(eqdata2$srch_adults_count)
eqdata2$srch_children_count <- as.factor(eqdata2$srch_children_count)
eqdata2$srch_room_count <- as.factor(eqdata2$srch_room_count)
eqdata2$srch_saturday_night_bool <- as.factor(eqdata2$srch_saturday_night_bool)
eqdata2$random_bool <- as.factor(eqdata2$random_bool)
eqdata2$booking_bool <- as.factor(eqdata2$booking_bool)
eqdata2$click_bool <- as.factor(eqdata2$click_bool)
gross_bookings_usdSafe = eqdata2$gross_bookings_usd
# was working but suddenly factor to int conversion is having issues
eqdata2$gross_bookings_usd <- gross_bookings_usdSafe
  #as.numeric(levels(eqdata2$gross_bookings_usd))
head(gross_bookings_usd22)
str(gross_bookings_usd22)
gross_bookings_usdtest = as.numeric.factor(gross_bookings_usdSafe)
str(gross_bookings_usdtest)
eqdata2$srch_booking_window <- as.numeric(eqdata2$srch_booking_window)
str(eqdata2)
head(eqdata2$srch_booking_window)
nrow(eqdata2)

# Randomly split data for a smaller set for fast processing
set.seed(88)
split = sample.split(eqdata2, SplitRatio = 0.03)
qualityTrain = subset(eqdata2, split == TRUE)
dataTest = subset(eqdata2, split == FALSE)
nrow(qualityTrain)
str(qualityTrain)
#Find Correlations of all the 25 numeric variables
cor()

#Random forest
varNames <- names(qualityTrain)
# Exclude ID or Response variable
varNames <- varNames[!varNames %in% c("date_time","click_bool","srch_id","site_id","gross_bookings_usd","visitor_location_country_id","prop_id","srch_destination_id","srch_destination_id")]
varNames
# add + sign between exploratory variables
varNames1 <- paste(varNames, collapse = "+")
  varNames1
# Add response variable and convert to a formula object
rf.form <- as.formula(paste("gross_bookings_usd", varNames1, sep = " ~ "))
rf.form
rJungle <- randomForest(rf.form, qualityTrain, ntree=50, importance=T)
rJungle2 <- randomForest(rf.form, eqdata2, ntree=10, importance=T)
plot(rJungle)
varImpPlot(rjungle)
#testdata = read.csv("test.csv")
#str(testdata)
Prediction <- predict(rJungle, dataTest)
rmse <- mean((dataTest$gross_bookings_usd - predictions)^2)
print(rmse)

#Linear Model
PointsReg <- lm(gross_bookings_usd ~ site_id+srch_adults_count+srch_children_count, data=grossdata, rm.na = TRUE)

summary(PointsReg)
summary(qualityTrain)
# Make predictions on test set
PointsPredictions = predict(PointsReg, newdata=dataTest)
# Compute out-of-sample R^2
SSE = sum((PointsPredictions - dataTest$gross_bookings_usd)^2)
SST = sum((mean(qualityTrain$gross_bookings_usd) - dataTest$gross_bookings_usd)^2)
R2 = 1 - SSE/SST
R2
# Compute the RMSE
RMSE = sqrt(SSE/nrow(dataTest))
RMSE

#Decision Tree
bk_tree <- rpart(booking_bool ~  prop_starrating + price_usd + prop_location_score1 + prop_location_score2 + promotion_flag + srch_room_count, data = eqdata2, method = "class")
printcp(bk_tree)
plotcp(bk_tree)
#prune(titanictree, cp= titanictree$cptable[which.min(titanictree$cptable[,"xerror"]),"CP"])
fancyRpartPlot(bk_tree)

# Logistic Regression Model
QualityLog = glm(booking_bool ~ prop_starrating + price_usd + prop_location_score1 + prop_location_score2 + promotion_flag + srch_room_count, data=dataTest, family=binomial)
summary(QualityLog)
# Make predictions on training set
predictTrain = predict(QualityLog, type="response")
# Analyze predictions
summary(predictTrain)
tapply(predictTrain, qualityTrain$PoorCare, mean)


StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, ntree=200, nodesize=25) #no. of trees and bucket size

######################################Mrunmayi################
#Cp complexity parameter: large cp means small trees, best soln had cp close to 0
#Randomize data
myraw <- read.csv("without _clickbool.csv",header=TRUE)
head(myraw)
rand <- runif(nrow(myraw)) 

myrawrand <- myraw[order(rand), ]

#Partition data
myrawrandtrain <- myrawrand[1:4000, ]
myrawrandtest <- myrawrand[4001:8634, ]

#Build decision tree
myrawrandtraintree <- rpart(booking_bool ~ ., data = myrawrandtrain, method = "class")
myrawrandtraintree
printcp(myrawrandtraintree)
?printcp
plotcp(myrawrandtraintree)
#prune(titanictree, cp= titanictree$cptable[which.min(titanictree$cptable[,"xerror"]),"CP"])
fancyRpartPlot(myrawrandtraintree)

#Evaluate tree performance
myrawrandtrain$treepred <- predict(myrawrandtraintree, myrawrandtrain, type = "class")
table(Actual = myrawrandtrain$booking_bool, Predicted = myrawrandtrain$treepred)
tree.sse = sum((myrawrandtrain$treepred - myrawrandtrain$booking_bool)^2)
tree.sse

myrawrandtrain$treepredcorrect <- myrawrandtrain$booking_bool == myrawrandtrain$treepred 
traintreecorrectcount <- length(which(myrawrandtrain$treepredcorrect))
traintreeincorrectcount <- nrow(myrawrandtrain) - traintreecorrectcount
traintreeerrorrate <- traintreeincorrectcount/nrow(myrawrandtrain)
traintreeaccuracy <- 1-traintreeerrorrate

myrawrandtest$treepred <- predict(myrawrandtraintree, myrawrandtest, type = "class")
table(Actual = myrawrandtest$booking_bool, Predicted = myrawrandtest$treepred)

myrawrandtest$treepredcorrect <- myrawrandtest$booking_bool == myrawrandtest$treepred 
testtreecorrectcount <- length(which(myrawrandtest$treepredcorrect))
testtreeincorrectcount <- nrow(myrawrandtest) - testtreecorrectcount
testtreeerrorrate <- testtreeincorrectcount/nrow(myrawrandtest)
testtreeaccuracy <- 1-testtreeerrorrate

#Compare
paste("TREE TRAIN: Error Rate (", traintreeerrorrate, ") Accuracy (", traintreeaccuracy, ")")
paste("TREE TEST: Error Rate (", testtreeerrorrate, ") Accuracy (", testtreeaccuracy, ")")
