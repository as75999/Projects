#APM project

library(plyr)
library(stringr)
library(doBy)
library(zoo)
library(dplyr)
library(rpart)
library(caret)
library(randomForest)
library(gbm)
library(sqldf)
library(ROCR)
library(e1071)
library(MLmetrics)


data_train_pre=read.csv('D:/Fall/APM/project/data_train_pre.csv')
data_test_pre=read.csv('D:/Fall/APM/project/data_test_pre.csv')


func = data_train_pre[data_train_pre$status_group == 'functional',]
non_func = data_train_pre[data_train_pre$status_group == 'non functional',]
func_needs_repair = data_train_pre[data_train_pre$status_group == 'functional needs repair',]
str(data_train_pre)


func_needs_repair = rbind(func_needs_repair,sample_n(func,2158))
data_train_sampled = rbind(func_needs_repair,sample_n(non_func,2159))
dim(data_train_sampled)
str(data_train_sampled)

#########################
###################only with test
x=data_train_pre
levels(x$scheme_management)
levels(data_test_pre$scheme_management)
x$scheme_management[x$scheme_management=="None"] <- ""
x$scheme_management <- factor(as.character(x$scheme_management))
x$extraction_type[x$extraction_type=="other - mkulima/shinyanga"] <- ""
x$extraction_type <- factor(as.character(x$extraction_type))


############################################


################Random Forest########################


RF100 = randomForest(status_group ~ .-lga-date_recorded-extraction_type, data = data_train_sampled, ntree=100)
pred_rf_prob = predict(RF100, data_train_pre, type = "prob")
pred_rf_response = predict(RF100, data_train_pre, type = "response")
confusionMatrix(pred_rf_response,data_train_pre$status_group)
data_train_pre$twoclass = ifelse(data_train_pre$status_group == "functional needs repair", "functional needs repair", "func/non func")
data_train_pre$twoclass=as.factor(data_train_pre$twoclass)
rf.roc <- prediction(pred_rf_prob[,2],data_train_pre$twoclass)
rf.auc <- performance(rf.roc, 'tpr', 'fpr', cost.fp = 100, cost.fn = 1) 
plot(rf.auc) 
cutoffs = data.frame(cut = rf.auc@alpha.values[[1]], fpr = rf.auc@x.values[[1]], tpr = rf.auc@y.values[[1]])
cutoffs = cutoffs[order(cutoffs$tpr, decreasing = TRUE),]
head(subset(cutoffs, fpr < 0.1))
b<-array()
thres = 0.73
b <- ifelse(pred_rf_prob[,2]>thres, "functional needs repair", ifelse(pred_rf_prob[,1] >pred_rf_prob[,3],"functional", "non functional"))
c <- data_train_pre$status_group
confusionMatrix(b,c)


b1 <- ifelse(b == "functional", 1, ifelse(b == "functional needs repair", 2, 3))
c1 <- ifelse(c == "functional", 1, ifelse(c == "functional needs repair", 2, 3)) 

F1_Score(y_true = b1, y_pred = c1,positive = "2")
write.csv(cutoffs,'D:/Fall/APM/project/RF_custoffs.csv')


####################### GBM ##########################
x=data_train_sampled[,-c(41)]
y=data_train_sampled[,41]


#data_train_pre$employment_rate=as.integer(data_train_pre$employment_rate)
#data_train_pre$popdeath_rate=as.integer(data_train_pre$popdeath_rate)
#data_train_pre$crime_rating=as.integer(data_train_pre$crime_rating)

ntrees = 500
moZd      
el_gbm = gbm.fit(
  x = x, 
  y = y,
  distribution = "multinomial",
  n.trees = ntrees,
  interaction.depth = 6,
  shrinkage = .01,
  n.minobsinnode = 10,
  verbose = TRUE)
##model_gbm = gbm(status_group~.-lga-date_recorded-extraction_type-twoclass,
##  distribution = "multinomial",
##  data=data_train_pre,
##  n.trees = 100,
#3 interaction.depth = 6,
##  shrinkage = .01,
  )

pred_gbm_prob = predict(object=el_gbm, newdata=data_train_pre[,-c(43,41)],n.trees=gbm.perf(el_gbm, plot.it = FALSE), type = "response")
pred_gbm_prob=as.data.frame(pred_gbm_prob)
gbm.perf(model_gbm)
variable_importance=summary(model_gbm)
gbm.roc <- prediction(pred_gbm_prob[,2],data_train_pre$twoclass)
gbm.auc <- performance(gbm.roc, 'tpr', 'fpr', cost.fp = 100, cost.fn = 1) 
plot(gbm.auc) 
cutoffs = data.frame(cut = gbm.auc@alpha.values[[1]], fpr = gbm.auc@x.values[[1]], tpr = gbm.auc@y.values[[1]])
cutoffs = cutoffs[order(cutoffs$tpr, decreasing = TRUE),]
head(subset(cutoffs, fpr < 0.1))
b<-array()
thres = 0.667
b <- ifelse(pred_gbm_prob[,2]>thres, "functional needs repair", ifelse(pred_gbm_prob[,1] >pred_gbm_prob[,3],"functional", "non functional"))
c <- data_train_pre$status_group
confusionMatrix(b,c)
write.csv(cutoffs,'D:/Fall/APM/project/gbm_custoffs.csv')
write.csv(variable_importance,'D:/Fall/APM/project/variable_importance.csv')

library(MLmetrics)

b1 <- ifelse(b == "functional", 1, ifelse(b == "functional needs repair", 2, 3))
c1 <- ifelse(c == "functional", 1, ifelse(c == "functional needs repair", 2, 3)) 

F1_Score(y_true = b1, y_pred = c1,positive = "2")

############################################################################3


####################### Naive Bayes ##########################
x=data_train_sampled[,-c(41)]
y=data_train_sampled[,41]


#data_train_pre$employment_rate=as.integer(data_train_pre$employment_rate)
#data_train_pre$popdeath_rate=as.integer(data_train_pre$popdeath_rate)
#data_train_pre$crime_rating=as.integer(data_train_pre$crime_rating)


model_naive_bayes = naiveBayes(
  x = x, 
  y = y,
 )

pred_naive_prob = predict(model_naive_bayes, data_train_pre, type = "raw")
pred_naive_prob=as.data.frame(pred_naive_prob)


naive.roc <- prediction(pred_naive_prob[,2],data_train_pre$twoclass)
naive.auc <- performance(naive.roc, 'tpr', 'fpr', cost.fp = 100, cost.fn = 1) 
plot(naive.auc) 
cutoffs = data.frame(cut = naive.auc@alpha.values[[1]], fpr = naive.auc@x.values[[1]], tpr = naive.auc@y.values[[1]])
cutoffs = cutoffs[order(cutoffs$tpr, decreasing = TRUE),]
head(subset(cutoffs, fpr < 0.1))
b<-array()
thres = 0.995
b <- ifelse(pred_naive_prob[,2]>thres, "functional needs repair", ifelse(pred_naive_prob[,1] >pred_naive_prob[,3],"functional", "non functional"))
c <- data_train_pre$status_group
confusionMatrix(b,c)
write.csv(cutoffs,'D:/Fall/APM/project/naive_custoffs.csv')

b1 <- ifelse(b == "functional", 1, ifelse(b == "functional needs repair", 2, 3))
c1 <- ifelse(c == "functional", 1, ifelse(c == "functional needs repair", 2, 3)) 

F1_Score(y_true = b1, y_pred = c1,positive = "2")


###############
library(monmlp)
x_matrix=model.matrix(~.-date_recorded,data=x)
x1_matrix=model.matrix(~.-date_recorded,data=data_train_pre)

table(sapply(x_matrix, is.numeric))
for
y=as.matrix(x)
y1 <- ifelse(y == "functional", 1, ifelse(y == "functional needs repair", 2, 3))
r <- monmlp.fit(x_matrix, y1, hidden1=3, n.ensemble=15, monotone=1, bag=TRUE)
pred_mlp <- monmlp.predict(x =x_matrix , weights = r)
table(pred_mlp[,3])
## Plot ensemble mean
lines(x, p.mon, col = "blue", lwd = 3)
mlp.roc <- prediction(pred_mlp_prob[,2],data_train_pre$twoclass)
mlp.auc <- performance(mlp.roc, 'tpr', 'fpr', cost.fp = 100, cost.fn = 1) 
plot(mlp.auc) 
cutoffs = data.frame(cut = mlp.auc@alpha.values[[1]], fpr = mlp.auc@x.values[[1]], tpr = mlp.auc@y.values[[1]])
cutoffs = cutoffs[order(cutoffs$tpr, decreasing = TRUE),]
head(subset(cutoffs, fpr < 0.1))
b<-array()
thres = 0.995
b <- ifelse(pred_mlp_prob[,2]>thres, "functional needs repair", ifelse(pred_mlp_prob[,1] >pred_mlp_prob[,3],"functional", "non functional"))
c <- data_train_pre$status_group
confusionMatrix(b,c)
write.csv(cutoffs,'D:/Fall/APM/project/mlp_custoffs.csv')

b1 <- ifelse(b == "functional", 1, ifelse(b == "functional needs repair", 2, 3))
c1 <- ifelse(c == "functional", 1, ifelse(c == "functional needs repair", 2, 3)) 

F1_Score(y_true = b1, y_pred = c1,positive = "2")

#################### SVM ##########################

library(e1071)

svm.model = svm(status_group ~ .-longitude-latitude-lga-date_recorded-extraction_type, data = x, kernel = "linear", probability = TRUE)
svm.pred = predict(svm.model, data_train_pre[,-which(colnames(data_train_pre)=="status_group")], probability = TRUE, ddecision.values = TRUE)

table(svm.pred, data_train_pre$status_group)

# compute decision values and probabilities:
confusionMatrix(svm.pred,data_train_pre$status_group)

library(ROCR) 
svm.roc <- prediction(attributes(svm.pred)$decision.values, data_train_pre$status_group) 
svm.auc <- performance(svm.roc, 'tpr', 'fpr') 
plot(svm.auc) 


###rbf

svm.model = svm(status_group ~ .-longitude-latitude-lga-date_recorded-extraction_type, data = x, kernel = "radial")
svm.pred = predict(svm.model, data_train_pre[,-which(colnames(data_train_pre)=="status_group")])

table(svm.pred, data_train_pre$status_group)
confusionMatrix(svm.pred,data_train_pre$status_group)




###### H2o Random Forest


