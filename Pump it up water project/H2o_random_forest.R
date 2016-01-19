# Random forest with H2O

train=read.csv('D:/Fall/APM/project/data_train_pre.csv')
test=read.csv('D:/Fall/APM/project/data_test_pre.csv')

library(h2o)
localH2O = h2o.init()


predictors = c('lga','region', 'amount_tsh', 'funder', 'gps_height', 'installer',
               'longitude', 'latitude', 'wpt_name', 'num_private', 'basin', 'lga',
               'population', 'public_meeting', 'scheme_management', 'permit',
               'extraction_type_class', 'management', 'payment', 'water_quality',
               'quantity', 'source', 'source_class', 'waterpoint_type',
               'waterpoint_type_group', 'age', 'loc_type', 'employment_rate',
               'popdeath_rate', 'status_group', 'crime_rating')
target = "status_group"


trainHex = as.h2o(train, destination_frame = "train.hex")
testHex = as.h2o(test, destination_frame = "test.hex")



rfHex = h2o.randomForest(
  x = predictors,
  y = target,
  training_frame = trainHex,
  model_id = "rf_ntrees1000",
  ntrees = 1000, mtries = 10,
  seed = 123456) 


h2o.confusionMatrix(rfHex)


predictions = as.data.frame(h2o.predict(rfHex,testHex))[,1]

test$predictions=predictions
test1=test[,c('id','predictions')]
submission = tbl_dt(fread("data/SubmissionFormat.csv")) %>%
  mutate(status_group = predictions)
write.csv(test1,row.names = FALSE,quote = FALSE,
          file = "E:/submission-h2o_randomForest-ntrees1000.csv")
