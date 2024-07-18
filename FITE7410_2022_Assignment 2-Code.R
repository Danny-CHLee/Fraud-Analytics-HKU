library(readr)
library(skimr)
library(randomForest)
library(caret)
library(dplyr)
library(dataPreparation)
library(xgboost)
library(e1071)   
library(DMwR)
library(varhandle)

hw2 <- read_csv("/Users/HKU/Desktop/HKU/2022 SEM1/Fraud/Assignment 2/insurance_claims.csv")

skim(hw2) 
str(hw2)

table(hw2$fraud_reported)
round(prop.table(table(hw2$fraud_reported)),digits=3) #0.75 non-fraud, 0.25 fraud

colSums(is.na(hw2))
a <- table(hw2$police_report_available)
as.numeric(a[names(a)=="?"])
hw2 <- hw2 %>%
  dplyr::select(-c("_c39", "policy_number","insured_zip"))

#Simplify incident location
Drive <- str_detect(hw2$incident_location, "Drive")
hw2[Drive,]$incident_location <- "Drive"
Hwy <- str_detect(hw2$incident_location, "Hwy")
hw2[Hwy,]$incident_location <- "Hwy"
Lane <- str_detect(hw2$incident_location, "Lane")
hw2[Lane,]$incident_location <- "Lane"
Ave <- str_detect(hw2$incident_location, "Ave")
hw2[Ave,]$incident_location <- "Ave"
Ridge <- str_detect(hw2$incident_location, "Ridge")
hw2[Ridge,]$incident_location <- "Ridge"
St <- str_detect(hw2$incident_location, "St")
hw2[St,]$incident_location <- "St"




hw2$fraud_reported=ifelse(hw2$fraud_reported=="N",0,1)
hw2_encoding <- build_target_encoding(hw2, cols_to_encode = c( "policy_state","policy_csl","policy_deductable","insured_sex","insured_education_level","insured_occupation","insured_hobbies","insured_relationship",
                                                               "incident_type","collision_type","incident_severity","authorities_contacted","incident_state","incident_city","incident_location",
                                                               "incident_hour_of_the_day","number_of_vehicles_involved","property_damage","bodily_injuries","witnesses","police_report_available","auto_make","auto_model",
                                                               "auto_year"),
                                         target_col = "fraud_reported", functions = "mean")

hw2 <- target_encode(hw2, target_encoding = hw2_encoding)
hw2 <- data.frame(hw2)
hw2 <- hw2 %>% 
  dplyr::select(-c( "policy_state","policy_csl","policy_deductable","insured_sex","insured_education_level","insured_occupation","insured_hobbies","insured_relationship",
                     "incident_type","collision_type","incident_severity","authorities_contacted","incident_state","incident_city","incident_location",
                     "incident_hour_of_the_day","number_of_vehicles_involved","property_damage","bodily_injuries","witnesses","police_report_available","auto_make","auto_model",
                     "auto_year"))

hw2$policy_bind_date <- as.numeric(hw2$policy_bind_date) #change date into sequence of days
hw2$incident_date <- as.numeric(hw2$incident_date)
View(hw2)


########################################################################################################################
set.seed(36)
sample <- sample(c(TRUE, FALSE), nrow(hw2), replace=TRUE, prob=c(0.7,0.3))
train  <- hw2[sample, ]
test_original   <- hw2[!sample, ]

train$fraud_reported <- as.factor(train$fraud_reported)
set.seed(36)
train_SMOTE <- DMwR::SMOTE(fraud_reported ~ ., data=train)
table(train$fraud_reported)
table(train_SMOTE$fraud_reported)
table(test_original$fraud_reported)
train_SMOTE$fraud_reported <- unfactor(train_SMOTE$fraud_reported)
train_original <- train
# #change into factors
# col_names <- c( "policy_state","policy_csl","policy_deductable","insured_sex","insured_education_level","insured_occupation","insured_hobbies","insured_relationship",
#                 "incident_type","collision_type","incident_severity","authorities_contacted","incident_state","incident_city","incident_location",
#                 "incident_hour_of_the_day","number_of_vehicles_involved","property_damage","bodily_injuries","witnesses","police_report_available","auto_make","auto_model",
#                 "auto_year")
# train[col_names] <- lapply(train[col_names] , factor)
# str(train)
#########################################################################################################################
#XGBoost

train <- train_SMOTE
test <- test_original

new_tr = data.matrix(train[,!(colnames(train)=="fraud_reported")])
labels = train$fraud_reported

new_te = data.matrix(test[,!(colnames(test)=="fraud_reported")])
ts_label = test$fraud_reported

dtrain = xgb.DMatrix(data=new_tr, label=labels)
dtest = xgb.DMatrix(data=new_te, label=ts_label)

set.seed(36)
params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.3, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)
xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 200, nfold = 5, showsd = T, stratified = T, print.every.n = 10, early.stop.round = 20, maximize = F)
xgb_learn_val <- data.frame(iter=xgbcv[["evaluation_log"]][["iter"]],error=xgbcv[["evaluation_log"]][["test_logloss_mean"]],mean_log_loss="validation")
xgb_learn_train <- data.frame(iter=xgbcv[["evaluation_log"]][["iter"]],error=xgbcv[["evaluation_log"]][["train_logloss_mean"]],mean_log_loss="train")
xgb_learn <- rbind(xgb_learn_val, xgb_learn_train)
xgb_learn$mean_log_loss <- as.factor(xgb_learn$mean_log_loss)
ggplot() + geom_line(data = xgb_learn, aes(x = iter, y = error,color=mean_log_loss))+ xlab('number of iteration') + ylab('error') + labs(title = "XGBoost Log loss")
set.seed(36)
xgb1 <- xgb.train (params = params, data = dtrain, nrounds = 20, watchlist = list(val=dtest,train=dtrain), print.every.n = 10, early.stop.round = 10, maximize = F , eval_metric = "error")
xgb_results_val <- data.frame(iter=xgb1[["evaluation_log"]]$iter,error=xgb1[["evaluation_log"]]$val_error,log_loss="test")
xgb_results_train <- data.frame(iter=xgb1[["evaluation_log"]]$iter,error=xgb1[["evaluation_log"]]$train_error,log_loss="train")
xgb_results <- rbind(xgb_results_val, xgb_results_train)
ggplot() + geom_line(data = xgb_results, aes(x = iter, y = error,color=log_loss))+ xlab('number of iteration') + ylab('error') + labs(title = "XGBoost Classification error")

xgbpred <- predict (xgb1,dtest)
xgbpred <- ifelse (xgbpred > 0.5,1,0)
xgbpred <- as.factor(xgbpred)
ts_label <- as.factor(ts_label)
confusionMatrix (xgbpred, ts_label, mode = "everything", positive="1")

mat <- xgb.importance (feature_names = colnames(new_tr),model = xgb1)
xgb.plot.importance (importance_matrix = mat[1:20]) 

xgbpred_train <- predict (xgb1,dtrain)
xgbpred_train <- ifelse (xgbpred_train > 0.5,1,0)
xgbpred_train <- as.factor(xgbpred_train)
labels <- as.factor(labels)
confusionMatrix (xgbpred_train, labels, mode = "everything", positive="1")



#tuning parameters
library(mlr)
library(parallel)
library(parallelMap)
#create tasks
train$fraud_reported <- as.factor(train$fraud_reported)
test$fraud_reported <- as.factor(test$fraud_reported)
traintask <- makeClassifTask (data = train,target = "fraud_reported")
testtask <- makeClassifTask (data = test,target = "fraud_reported")
lrn <- makeLearner("classif.xgboost",predict.type = "response")
lrn$par.vals <- list( objective="binary:logistic", eval_metric="error", nrounds=100L, eta=0.05)
params <- makeParamSet( makeDiscreteParam("booster",values = c("gbtree","gblinear")),
                        makeIntegerParam("max_depth",lower = 3L,upper = 30L),
                        makeNumericParam("min_child_weight",lower = 1L,upper = 10L),
                        makeNumericParam("subsample",lower = 0.5,upper = 1),
                        makeNumericParam("colsample_bytree",lower = 0.5,upper = 1))
rdesc <- makeResampleDesc("CV",stratify = T,iters=5L)
ctrl <- makeTuneControlRandom(maxit = 10L)
parallelStartSocket(cpus = detectCores())
set.seed(36)
mytune <- tuneParams(learner = lrn, task = traintask, resampling = rdesc, measures = acc, par.set = params,
                     control = ctrl, show.info = T)
data.frame(mytune$x)
mytune$y
lrn_tune <- setHyperPars(lrn,par.vals = mytune$x)
set.seed(36)
xgmodel <- train(learner = lrn_tune,task = traintask)
xgpred <- predict(xgmodel,testtask)
XGB_eval <- confusionMatrix(xgpred$data$response,xgpred$data$truth, mode = "everything", positive="1")
XGB_eval

##########################################################################################################################
#Random forest
library(caTools)
library(class)
library(ROCR)

train <- train_SMOTE
test <- test_original
train$fraud_reported <- as.factor(train$fraud_reported)
test$fraud_reported <- as.factor(test$fraud_reported)
#separating the data into features(x) and ground truth (y) -training data
x_train = train[,!(colnames(train)=="fraud_reported")]
y_train = train$fraud_reported

#separating the data into features(x) and ground truth (y) -testing data
x_test = test[,!(colnames(test)=="fraud_reported")]
y_test = test$fraud_reported

# Feature Scaling
train_scale <- data.frame(scale(x_train))
test_scale <- data.frame(scale(x_test))

# Fitting KNN Model 
# to training dataset
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = y_train,
                      k = 1)
classifier_knn

# Confusiin Matrix
cm <- confusionMatrix(classifier_knn,y_test, mode = "everything", positive="1")
cm

# Calculate out of Sample error
misClassError <- mean(classifier_knn != y_test)
print(paste('Accuracy =', 1-misClassError))


#parametric tuning
knnGrid <-  expand.grid(k = c(1:20))
Probs = TRUE
fitControl <- trainControl(method = "repeatedcv", 
                           repeats = 3, 
                           classProbs = Probs,
                           summaryFunction = twoClassSummary)
train_scale$fraud_reported <- y_train
train_scale$fraud_reported=ifelse(train_scale$fraud_reported==0,"No","Yes")
knnFit <- caret::train(fraud_reported~., # formula
                       data = train_scale, # train data   
                       method = "knn",  
                       trControl = fitControl, 
                       tuneGrid = knnGrid,
                       ## Specify which metric to optimize
                       metric = "ROC")

knnFit
plot(knnFit)
train_scale$fraud_reported=ifelse(train_scale$fraud_reported=="No",0,1)
train_scale = train_scale[,!(colnames(train)=="fraud_reported")]



knnPredict <- predict(knnFit,newdata = test_scale)
knnPredict=ifelse(knnPredict=="No",0,1)
knnPredict
knn_eval <- confusionMatrix(as.factor(knnPredict),as.factor(y_test), mode = "everything", positive="1")
knn_eval

train <- train_SMOTE
x_train = train[,!(colnames(train)=="fraud_reported")]
y_train = train$fraud_reported
train_scale <- data.frame(scale(x_train))
knnPredict_train <- predict(knnFit,newdata = train_scale)
knnPredict_train=ifelse(knnPredict_train=="No",0,1)
knnPredict_train
confusionMatrix(as.factor(knnPredict_train),as.factor(y_train), mode = "everything", positive="1")



###########################################################################################################
XGBPredict <- unfactor(xgpred$data$response)
XGBtest <- unfactor(xgpred$data$truth)
ROCPred_XGB <- prediction(XGBPredict,XGBtest) 
ROCPer_XGB <- ROCR::performance(ROCPred_XGB, measure = "tpr", 
                                x.measure = "fpr")

auc_XGB <- ROCR::performance(ROCPred_XGB, measure = "auc")
auc_XGB <- auc_XGB@y.values[[1]]
auc_XGB

# Plotting curve
plot(ROCPer_XGB)
plot(ROCPer_XGB, colorize = TRUE, 
     print.cutoffs.at = seq(0.1, by = 0.1), 
     main = "ROC CURVE")
abline(a = 0, b = 1)

auc_XGB <- round(auc_XGB, 4)
legend(.6, .4, auc_XGB, title = "auc_XGB", cex = 1)




ROCPred_knn <- prediction(knnPredict, y_test) 
ROCPer_knn <- ROCR::performance(ROCPred_knn, measure = "tpr", 
                      x.measure = "fpr")

auc_knn <- ROCR::performance(ROCPred_knn, measure = "auc")
auc_knn <- auc_knn@y.values[[1]]
auc_knn

# Plotting curve
plot(ROCPer_knn)
plot(ROCPer_knn, colorize = TRUE, 
     print.cutoffs.at = seq(0.1, by = 0.1), 
     main = "ROC CURVE")
abline(a = 0, b = 1)

auc_knn <- round(auc_knn, 4)
legend(.6, .4, auc_knn, title = "auc_knn", cex = 1)

#########################################################################
Fraud <- hw2$fraud_reported==1
hw2_fraud <- hw2[Fraud,]
amount <- mean(hw2_fraud$total_claim_amount)
amount
investigation <- 50564/12
investigation
cost_mat=matrix(c(0,investigation,amount,investigation),ncol=2)
cost_mat
cm_hw2_knn <- as.matrix(knn_eval[["table"]])
cm_hw2_XGB <- as.matrix(XGB_eval[["table"]])
XGB_cost <- sum(cost_mat*cm_hw2_XGB)
XGB_cost
knn_cost <- sum(cost_mat*cm_hw2_knn)
knn_cost
(XGB_cost-knn_cost)/knn_cost
