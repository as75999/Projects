# Random forest with H2O
library(DMwR)
library(h2o)
library(ROCR)
library(rpart)
library(plyr)
library(caret)
library(MLmetrics)
train=read.csv('D:/Fall/APM/project/data_train_pre.csv')

test=read.csv('D:/Fall/APM/project/data_test_pre.csv')
train$month=as.factor(train$month)
test$month=as.factor(test$month)
train$y=ifelse(train2$status_group == "functional", 1, 0)
tr=SMOTE(y~., train, perc.over = 5118, k = 5)
length=nrow(train)
s=sample(length, length/2)
train2 <- train[s,]
train1 <- train[-s,]
s1=nrow(train2)
s2=sample(s1, s1/2)
train3 <- train2[-s2,]
train2 <- train2[s2,]


localH2O = h2o.init()
table(train$scheme_name)



hist(log(train$population))
hist(train$gps_height)
#boxplot(log(train$population)~train$status_group)

predictors = c('lga','region','month', 'amount_tsh', 'funder', 'gps_height', 'installer',
               'longitude', 'latitude', 'wpt_name', 'num_private', 'basin', 'lga',
               'population', 'public_meeting', 'scheme_management', 'permit',
               'extraction_type_class','extraction_type', 'management', 'payment', 'water_quality',
               'quantity', 'source', 'source_class', 'waterpoint_type',
               'waterpoint_type_group', 'age', 'loc_type') 
######'employment_rate',
######'####3            'popdeath_rate', 'crime_rating')
target = "status_group"
trainHex = as.h2o(train, destination_frame = "train.hex")
testHex = as.h2o(test, destination_frame = "test.hex")
train1Hex = as.h2o(train1, destination_frame = "train1.hex")
train2Hex = as.h2o(train2, destination_frame = "train2.hex")
train3Hex = as.h2o(train3, destination_frame = "train3.hex")


rfHex = h2o.randomForest(
  x = predictors,
  y = target,
  training_frame = train1Hex,
  model_id = "rf_ntrees1000",
  mtries=12,
  ntrees = 1000,
  ##balance_classes=TRUE,
  nbins=3,
  max_depth=25,
  seed = 123456,score_each_iteration=TRUE,stopping_metric='misclassification') 

cf=h2o.confusionMatrix(rfHex)
predictions_train = as.data.frame(h2o.predict(rfHex,train2Hex))
predictions_train$twoclass = ifelse(train2$status_group == "functional", 1, 0)

gbm.roc <- prediction(predictions_train$functional,predictions_train$twoclass)
gbm.auc <- performance(gbm.roc, 'tpr', 'fpr') 
plot(gbm.auc) 
cutoffs = data.frame(cut = gbm.auc@alpha.values[[1]], fpr = gbm.auc@x.values[[1]], tpr = gbm.auc@y.values[[1]])
cutoffs = cutoffs[order(cutoffs$tpr, decreasing = TRUE),]
head(subset(cutoffs, fpr < 0.3))
b<-array()
thres = 0.58
predicted_train <- ifelse(predictions_train$functional>thres, "functional", ifelse(predictions_train$functional.needs.repair >predictions_train$non.functional,"functional needs repair", "non functional"))
actual <- train$status_group
confusionMatrix(predicted_train,actual)


predictions = as.data.frame(h2o.predict(rfHex,train3Hex))
thres = 0.4
predicted <- ifelse(predictions$functional>thres, "functional", ifelse(predictions$functional.needs.repair >predictions$non.functional,"functional needs repair", "non functional"))
actualt2 <- train3$status_group
confusionMatrix(predicted,actualt2)

test$predictions=predicted
test1=test[,c('id','predictions')]
write.csv(test1,row.names = FALSE,quote = FALSE,
          file = "E:/submission-h2o_randomForest-ntrees1000.csv")

h2o.mse(rfHex)
h2o.r2(rfHex)
0.8217601
0.82199
300 0.6727617
0.833308
View(h2o.varimp(rfHex))
h2o.accuracy(cf)
