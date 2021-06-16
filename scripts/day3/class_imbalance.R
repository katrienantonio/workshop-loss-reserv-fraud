##############################################
#  Prep work
##############################################

library(caret)
library(kernlab)
library(performanceEstimation)
library(tidyverse)

?ticdata  # now that is a nice Dutch data set ;-)

data(ticdata)

##############################################
#  Play with class imbalanced data
##############################################

# the outcome, whether the customer purchased caravan insurance is 
# highly unbalanced
# predictors consist of:
# 	- customer subtype designation
#	  - demographic factors, derived from data at zip code level
#  	- product ownership information
# 85 predictors in total

dim(ticdata)
str(ticdata)
levels(ticdata$CARAVAN)

## which percentage of the clients does buy caravan insurance?
table(ticdata$CARAVAN)
prop.table(table(ticdata$CARAVAN))

# we create two (stratified) data sets: 
# 	training, test
# we use stratified random sampling
# 	- a training set that will be used to estimate model parameters, tuning models
#	  - a customer test set, solely for final evaluations of the models

set.seed(156)
split <- createDataPartition(ticdata$CARAVAN, p = .7)[[1]]
length(split) # this is approx equal to?

# we check the training set
test    <- ticdata[- split,]
dim(test)
training  <- ticdata[split,]
dim(training)

# check imbalance in training set
# DIY


# the 'caret' package has two functions 'downSample' and 'upSample'
# that readjust the class frequencies
# downSample
set.seed(1237)
down_sampled <- downSample(training[, -ncol(training)], training$CARAVAN)
table(down_sampled$Class)  # do you recognize the number?

# upSample
set.seed(1237)
up_sampled <- upSample(training[, -ncol(training)], training$CARAVAN)
table(up_sampled$Class)  # do you recognize the number?

# SMOTE
set.seed(1237)
smoted <- smote(CARAVAN ~ ., data = training)
table(smoted$CARAVAN)

# SMOTE has some parameters that can be used like this (see ? SMOTE)
smoted_2 <- smote(CARAVAN ~ . , data = training, k = 5, perc.over = ___)

# now start building your classifier on the rebalanced data, BUT evaluate on the test!

