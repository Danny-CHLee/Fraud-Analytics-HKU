# FITE7410 Financial Fraud Analytics
# Assignment 1
# Lee Chung Ho 3036030037

# Part 1 EDA
#install and load package readr to improt data
install.packages("readr","ggplot2","plotly","psych")
install.packages("cowplot")
install.packages("skimr")
install.packages("tidyverse")
install.packages("dplyr")
library(cowplot)
library(readr)
library(psych)
library(ggplot2)
library(dplyr)
library(plotly)
library(skimr)
library(tidyverse)
library(data.table)
library(corrplot)
library(RColorBrewer)

#read the PaySim data
PaySim <- read_csv("/Users/HKU/Desktop/HKU/2022 SEM1/Fraud/Assignment 1/PS_20174392719_1491204439457_log.csv")
#display the fisrt 10 rows of data
head(PaySim,10)

#Exploratory Data Analysis (EDA)
#EDA – Step 1: Distinguish Attributes -> understand the attributes
#Print the attribute names and data types
str(PaySim)

#Obtain the basic stats of the attributes
summary(PaySim)

#Explain the attributes
# step: time step in hour, num
# type: transaction type: PAYMENT, TRANSFER, CASH_OUT, DEBIT, CASH_IN, chr
# amount: transaction amount, num
# nameOrig: customer code, chr
# oldbalanceOrg: intial balance before the transaction, num
# newbalanceOrig: new balance after the transaction, num
# nameDest: transaction recipient code, chr
# oldbalanceDest: initial balance of recipient before the transaction, num
# newbalanceDest: the new balance of recipient after the transaction, num
# isFraud: fraud transactions (empty the funds by transferring to another account and then cashing out of the system) , 0 or 1
# isFlaggedFraud: a business model flagged illegal attempts to transfer more than 200.000 in a single transaction as Frauds, 0 or 1




#EDA – Step 2: Univariate Analysis
#look into the fraud and non-fraud ratio
#table(PaySim$isFraud, PaySim$isFlaggedFraud)
#prop.table(table(PaySim$isFraud))
fraud <- sum(PaySim$isFraud) #Find number of frauds
non_fraud <- 6362620-fraud #Total number of entries - number of frauds 
slices <- c(fraud,non_fraud) 
lbls <- c("Fraud", "Non-fraud")
pct <- round(slices/sum(slices)*100,3) #calculate the percentages
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # add % to labels
pie(slices,labels = lbls,
    main="(a) Fraud and non-fraud ratio")
"This is a very unbalanced dataset"

#Distribution of Transaction types
type_table <- data.frame(table(PaySim$type))
colnames(type_table)[1] = "Type"
colnames(type_table)[2] = "Number_of_transactions"
type_table 
bar_1 <- ggplot(type_table , aes(x=Type, y=Number_of_transactions))+
  geom_bar(stat="identity", width=1, fill="steelblue") +
  geom_text(aes(label=Number_of_transactions), vjust=-0.3, color="black", size=3.5)+ ggtitle('(b)Distribution of Transaction types')+theme_minimal()
  
bar_1

# PaySim$step
# hi_a <- ggplot(data.frame(step=PaySim$step), aes(x=step)) + 
#   geom_histogram(color="black", fill="white")
# hi_b <- ggplot(data.frame(amount=PaySim$amount), aes(x=amount)) + 
#   geom_histogram(color="black", fill="white")
# hi_c <- ggplot(data.frame(oldbalanceOrg=PaySim$oldbalanceOrg), aes(x=oldbalanceOrg)) + 
#   geom_histogram(color="black", fill="white")
# hi_d <- ggplot(data.frame(newbalanceOrig=PaySim$newbalanceOrig), aes(x=newbalanceOrig)) + 
#   geom_histogram(color="black", fill="white")
# hi_e <- ggplot(data.frame(oldbalanceDest=PaySim$oldbalanceDest), aes(x=oldbalanceDest)) + 
#   geom_histogram(color="black", fill="white")
# hi_f <- ggplot(data.frame(newbalanceDest=PaySim$newbalanceDest), aes(x=newbalanceDest)) + 
#   geom_histogram(color="black", fill="white")
# hi_g <- ggplot(data.frame(isFraud=PaySim$isFraud), aes(x=isFraud)) + 
#   geom_histogram(color="black", fill="white")
# hi_h <- ggplot(data.frame(isFlaggedFraud=PaySim$isFlaggedFraud), aes(x=isFlaggedFraud)) + 
#   geom_histogram(color="black", fill="white")
# plot_grid(hi_a, hi_b, hi_c, hi_d, hi_e, hi_f, hi_g, hi_h,
#           labels = c("1", "2", "3", "4", "5", "6", "7", "8"),
#           ncol = 3, nrow = 3)






#EDA – Step 3: Bi-/Multi-variate Analysis
#plot the relation between types and fraud
T_and_F <- data.frame(type=PaySim$type, isFraud=PaySim$isFraud)
T_and_F_table <- data.frame(table(T_and_F))
# T_and_F_table <- cbind(T_and_F_table, c(T_and_F_table[1,2]+T_and_F_table[1,1], T_and_F_table[2,2]+T_and_F_table[2,1], T_and_F_table[3,2]+T_and_F_table[3,1],T_and_F_table[4,2]+T_and_F_table[4,1],T_and_F_table[5,2]+T_and_F_table[5,1]))
# colnames(T_and_F_table)[1] = "Non-fraud"
# colnames(T_and_F_table)[2] = "Fraud"
# colnames(T_and_F_table)[3] = "Sum"
T_and_F_fraud <- T_and_F_table$isFraud==1
T_and_F_tab <- T_and_F_table[T_and_F_fraud,]
colnames(T_and_F_tab)[3] = "Number_of_Fraud"
T_and_F_tab
bar_2 <- ggplot(T_and_F_tab , aes(x=type, y=Number_of_Fraud))+
  geom_bar(stat="identity", width=1, fill="steelblue") +
  geom_text(aes(label=Number_of_Fraud), vjust=-0.3, color="black", size=3.5)+
  ggtitle('Distribution of fraud in transaction types')+theme_minimal()
bar_2

#time step and fraud relation
time_1 <- ggplot(data=PaySim, aes(x=step))+geom_histogram(bins=100, fill="black")+labs(x='Time in hours since first transaction', y='No. of tansactions') + ggtitle('Transactions over time')
time_3 <- ggplot(data=Fra, aes(x=step))+geom_histogram(bins=100, fill="red")+labs(x='Time in hours since first transaction', y='No. of fraud tansactions') + ggtitle('Fraudulent transactions over time')
time_2 <- ggplot(data=nonfra, aes(x=step))+geom_histogram(bins=100, fill="blue")+labs(x='Time in hours since first transaction', y='No. of non-fraud tansactions') + ggtitle('Non-fraudulent transactions over time')
plot_grid(time_1,time_2,time_3,
          labels = c("(a)", "(b)", "(c)"),
          ncol = 3, nrow = 1)
#table(PaySim$step)

#correlation plot between genuine transactions and Fraudulent transactions
cor_PaySim <-cor(PaySim[-c(2,4,7)])
corr_1 <- corrplot(cor_PaySim, type="lower", order="alphabet", title="(a) Attribute correlation of all transactions",
         col=brewer.pal(n=8, name="RdYlBu"), mar=c(0,0,1,0))
PaySimDT <- data.table(PaySim[-c(2,4,7)])
Fra <- PaySimDT[grepl('1',isFraud)]
nonfra <- PaySimDT[grepl('0',isFraud)]
cor_Fra <-cor(Fra)
cor_Fra[is.na(cor_Fra)] <- 0
cor_nonfra <-cor(nonfra)
cor_nonfra[is.na(cor_nonfra)] <- 0
corr_3 <- corrplot(cor_Fra, type="lower", order="alphabet",title="(c) Attribute correlation of fraudulent transactions",
         col=brewer.pal(n=8, name="RdYlBu"), mar=c(0,0,1,0))
corr_2 <- corrplot(cor_nonfra, type="lower", order="alphabet",title="(b) Attribute correlation of non-fraudulent transactions",
         col=brewer.pal(n=8, name="RdYlBu"), mar=c(0,0,1,0))






#EDA – Step 4: Detect aberrant & missing values
#Check missing values
skim(PaySim)
#Check NA values
sum(is.na(PaySim))
#Check if there are negative values
sum(PaySim<0)

# for testing 
# pppppp <- data.frame(x=c(2,5,-10,NA,7), y=c(81,-1001,-1,NA,-991))
# sum(is.na(pppppp))
# pppppp <- data.frame(x=c(2,5,-10,7), y=c(81,-1001,-1,-991))
# sum(pppppp<0)


#EDA – Step 5: Detect outliers
#box-and-whisker plot
PaySim$isFraud <- factor(PaySim$isFraud)
box_0 <- ggplot(data=data.frame(step=PaySim$step, isFraud=PaySim$isFraud), aes(y=step, x=isFraud, fill=isFraud)) + 
  geom_boxplot(outlier.colour = "red", outlier.shape = 1) + ggtitle("Time step in hour vs class") +
  ylab("Amount") + xlab("isFraud")+theme(plot.title = element_text(size = 10))
box_1 <- ggplot(data=data.frame(amount=PaySim$amount, isFraud=PaySim$isFraud), aes(y=amount, x=isFraud, fill=isFraud)) + 
   geom_boxplot(outlier.colour = "red", outlier.shape = 1) + ggtitle("Amount vs class") +
  ylab("Amount") + xlab("isFraud")+theme(plot.title = element_text(size = 10))
box_2 <- ggplot(data=data.frame(oldbalanceOrg=PaySim$oldbalanceOrg, isFraud=PaySim$isFraud), aes(y=oldbalanceOrg, x=isFraud, fill=isFraud)) + 
  geom_boxplot(outlier.colour = "red", outlier.shape = 1) + ggtitle("Intial balance of original account vs class") +
  ylab("oldbalanceOrg") + xlab("isFraud")+theme(plot.title = element_text(size = 10))
box_3 <- ggplot(data=data.frame(newbalanceOrig=PaySim$newbalanceOrig, isFraud=PaySim$isFraud), aes(y=newbalanceOrig, x=isFraud, fill=isFraud)) + 
  geom_boxplot(outlier.colour = "red", outlier.shape = 1) + ggtitle("New balance of original account vs class") +
  ylab("newbalanceOrig") + xlab("isFraud")+theme(plot.title = element_text(size = 10))
box_4 <- ggplot(data=data.frame(oldbalanceDest=PaySim$oldbalanceDest, isFraud=PaySim$isFraud), aes(y=oldbalanceDest, x=isFraud, fill=isFraud)) + 
  geom_boxplot(outlier.colour = "red", outlier.shape = 1) + ggtitle("Intial balance of recipient account vs class") +
  ylab("oldbalanceDest") + xlab("isFraud")+theme(plot.title = element_text(size = 10))
box_5 <- ggplot(data=data.frame(newbalanceDest=PaySim$newbalanceDest, isFraud=PaySim$isFraud), aes(y=newbalanceDest, x=isFraud, fill=isFraud)) + 
  geom_boxplot(outlier.colour = "red", outlier.shape = 1) + ggtitle("New balance of recipient account vs class") +
  ylab("newbalanceDest") + xlab("isFraud")+theme(plot.title = element_text(size = 10))
plot_grid(box_0,box_1, box_2,box_3, box_4, box_5,
          labels = c("(a)", "(b)", "(c)" , "(d)", "(e)", "(f)"),
          ncol = 3, nrow = 2)

# box_a <- ggplot(data.frame(step=PaySim$step), aes(x=step)) + 
#   geom_boxplot(outlier.colour = "red", outlier.shape = 1)
# box_b <- ggplot(data.frame(amount=PaySim$amount), aes(x=amount)) + 
#   geom_boxplot(outlier.colour = "red", outlier.shape = 1)
# box_c <- ggplot(data.frame(oldbalanceOrg=PaySim$oldbalanceOrg), aes(x=oldbalanceOrg)) + 
#   geom_boxplot(outlier.colour = "red", outlier.shape = 1)
# box_d <- ggplot(data.frame(newbalanceOrig=PaySim$newbalanceOrig), aes(x=newbalanceOrig)) + 
#   geom_boxplot(outlier.colour = "red", outlier.shape = 1)
# box_e <- ggplot(data.frame(oldbalanceDest=PaySim$oldbalanceDest), aes(x=oldbalanceDest)) + 
#   geom_boxplot(outlier.colour = "red", outlier.shape = 1)
# box_f <- ggplot(data.frame(newbalanceDest=PaySim$newbalanceDest), aes(x=newbalanceDest)) + 
#   geom_boxplot(outlier.colour = "red", outlier.shape = 1)
# box_g <- ggplot(data.frame(isFraud=PaySim$isFraud), aes(x=isFraud)) + 
#   geom_boxplot(outlier.colour = "red", outlier.shape = 1)
# box_h <- ggplot(data.frame(isFlaggedFraud=PaySim$isFlaggedFraud), aes(x=isFlaggedFraud)) + 
#   geom_boxplot(outlier.colour = "red", outlier.shape = 1)
# plot_grid(box_a, box_b, box_c, box_d, box_e, box_f,
#           labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"),
#           ncol = 3, nrow = 2)

#difference in correlation -> amount/newbalanceDest, amount/newbalanceOrig, amount/oldbalanceDest, amount/oldbalanceOrg
# abc <- ggplot(Fra, aes(x=amount, y=oldbalanceDest)) + geom_point()
# abcd <- ggplot(nonfra, aes(x=amount, y=oldbalanceDest)) + geom_point()
# plot_grid(abc, abcd, 
#           labels = c("(a)", "(b)"),
#           ncol = 2, nrow = 1)
# abcde <- ggplot(Fra, aes(x=amount, y=newbalanceOrig)) + geom_point() + geom_smooth(method=lm)
# abcdef <- ggplot(nonfra, aes(x=amount, y=newbalanceOrig)) + geom_point() + geom_smooth(method=lm)
# plot_grid(abcde, abcdef,
#           labels = c("(a)", "(b)"),
#           ncol = 2, nrow = 1)

#EDA – Step 6: Feature Engineering
#add day feature? step modulo 24
PaySim_day <- mutate(PaySimDT, time = step%%24)
Fra_day <- mutate(Fra, time = step%%24)
nonofra_day <- mutate(nonfra, time = step%%24)
day_1 <- ggplot(data=PaySim_day, aes(x=time))+geom_histogram(bins=100, fill="black")+labs(x='Time modulo 24', y='No. of tansactions') + ggtitle('Tansactions over time')
day_3 <- ggplot(data=Fra_day, aes(x=time))+geom_histogram(bins=100, fill="red")+labs(x='Time modulo 24', y='No. of fraud tansactions') + ggtitle('Fraud tansactions over time')
day_2 <- ggplot(data=nonofra_day, aes(x=time))+geom_histogram(bins=100, fill="blue")+labs(x='Time modulo 24', y='No. of non-fraud tansactions') + ggtitle('Non-fraud tansactions over time')
plot_grid(day_1,day_2,day_3,
          labels = c("(a)", "(b)", "(c)"),
          ncol = 3, nrow = 1)

#recode character variables to numeric
PaySim_copy <- PaySim
nameorig <- PaySim_copy %>% 
  group_by(nameOrig) %>% 
  summarise(nameOrig_total_trans = n()) %>% 
  mutate(nameOrig_ID = seq(1, 6353307, 1))
table(nameorig$nameOrig_total_trans)

type_num <- PaySim_copy %>% 
  group_by(type) %>% 
  summarise(type_total_trans = n()) %>% 
  mutate(type_ID = seq(1, 5, 1))
table(type_num$type, type_num$type_ID)

namedest <- PaySim_copy %>% 
  group_by(nameDest) %>% 
  summarise(nameDest_total_trans = n()) %>% 
  mutate(nameDest_ID = seq(1, 2722362, 1))
table(namedest$nameDest_total_trans)










#Part 2 clustering
#split train and test dataset
#plot scatter fill = class, amount against time
#use smote/ROSE
#find clusters
#find class in clusters
#visualize clusters
#fit test set into cluster
#name fraud and non-fraud clusters

install.packages("factoextra","cluster","caret")
install.packages("devtools")
install.packages("ROSE")
install.packages(c("zoo","xts","quantmod"))
install.packages("DMwR")
install.packages("DMwR")
install.packages("fdm2id")
install.packages("SparkR")
remotes::install_github("cran/DMwR")
devtools::install_github('apache/spark@v3.3.0', subdir='R/pkg')
install.packages("parameters")
library(readr)
library(tidyverse) # data manipulation 
library(devtools)
library(cluster) # clustering algorithms 
library(factoextra) # clustering algorithms & visualization
library(caret) # streamline model training process
library(ROSE)
library(DMwR)
library(fdm2id)
library(parameters)

#load the dataset
Credit <- read_csv("/Users/HKU/Desktop/HKU/2022 SEM1/Fraud/Assignment 1/creditcard.csv")
#check the attributes
str(Credit)

#splitting into train and test sets
sample_size <- floor(0.7*nrow(Credit))
sample_size
set.seed(123)
train_ind = sample(seq_len(nrow(Credit)), size=sample_size)
train <- Credit[train_ind, ]
test <- Credit[-train_ind, ]

#look at the class of the each sets
table(Credit$Class)
table(train$Class)
table(test$Class)

#use smote
train <- as.data.frame(train)
train$Class <- as.factor(train$Class)
set.seed(0)
smote_train <- SMOTE(Class ~ ., data=train)
table(smote_train$Class)

#use ROSE
set.seed(0)
rose_train <- ROSE(Class ~., data=train)$data
table(rose_train$Class)

#scale the datasets
scale_smote <- scale(smote_train[-c(1,31)])
scale_rose <- scale(rose_train[-c(1,31)])

#elbow
wss_smote <- (nrow(scale_smote)-1) *sum(apply(scale_smote,2,var))
for (i in 2:20) wss_smote[i] <- sum(kmeans(scale_smote,centers=i)$withinss)
plot(1:20,wss_smote,type="b",main="(a) Elbow method under SMOTE", xlab="Number of Clusters", ylab="Within groups sum of squares")
# 3

wss_rose <- (nrow(scale_rose)-1) *sum(apply(scale_rose,2,var))
for (i in 2:20) wss_rose[i] <- sum(kmeans(scale_rose,centers=i)$withinss)
plot(1:20,wss_rose,type="b",main="(b) Elbow method under ROSE", xlab="Number of Clusters", ylab="Within groups sum of squares")
#4

#fviz_nbclust(scale_rose, kmeans, method = "silhouette")


#form the k-means clusters
# kmeans_smote <- kmeans(scale_smote, centers=6, nstart=25)
kmeans_smote_3 <- kmeans(scale_smote, centers=3, nstart=25)
kmeans_rose <- kmeans(scale_rose, centers=4, nstart=25)

#visualize the clusters
# fviz_cluster(kmeans_smote, scale_smote, geom = "point",
#              palette = "Set2", ggtheme = theme_minimal())
fviz_cluster(kmeans_smote_3, scale_smote, geom = "point",main = "(a) SMOTE cluster visualisation",
             palette = "Set2", ggtheme = theme_minimal())
fviz_cluster(kmeans_rose, scale_rose, geom = "point",main = "(b) ROSE cluster visualisation",
             palette = "Set2", ggtheme = theme_minimal())

#Append the cluster number
# kmeans_smote_with_class <- as.data.frame(scale_smote) 
# kmeans_smote_with_class <- mutate(kmeans_smote_with_class, cluster = kmeans_smote$cluster)
# kmeans_smote_with_class <- mutate(kmeans_smote_with_class, fraud = smote_train$Class)
# table(kmeans_smote_with_class$cluster, kmeans_smote_with_class$fraud)  

kmeans_smote_3_with_class <- as.data.frame(scale_smote)
kmeans_smote_3_with_class <- mutate(kmeans_smote_3_with_class, cluster = kmeans_smote_3$cluster)
kmeans_smote_3_with_class <- mutate(kmeans_smote_3_with_class, fraud = smote_train$Class)
table(kmeans_smote_3_with_class$cluster, kmeans_smote_3_with_class$fraud)

kmeans_rose_with_class <- as.data.frame(scale_rose) 
kmeans_rose_with_class <- mutate(kmeans_rose_with_class, cluster = kmeans_rose$cluster)
kmeans_rose_with_class <- mutate(kmeans_rose_with_class, fraud = rose_train$Class)
table(kmeans_rose_with_class$cluster, kmeans_rose_with_class$fraud)  

#scale the test data
scale_test <- scale(test[-c(1,31)])

#function for defining cluster number of the test dataset with a built cluster model
predict.kmeans <- function(object, newdata , ...) {
  if (is.null(newdata)) {
    return(object$cluster)
  }
  
  # compute squared euclidean distance from each sample to each cluster center
  centers <- object$centers
  sumsquares_by_center <- apply(centers, 1, function(x) {
    colSums((t(newdata) - x)^2)
  })
  if (is.null(nrow(sumsquares_by_center))) {
    as.vector(which.min(sumsquares_by_center))
  } else {
    as.vector(apply(as.data.frame(sumsquares_by_center), 1, which.min))
  }
}


#Make predictions
# prediction_smote <- predict.kmeans(kmeans_smote, scale_test)
prediction_smote_3 <- predict.kmeans(kmeans_smote_3, scale_test)
prediction_rose <- predict.kmeans(kmeans_rose, scale_test)



#Append the cluster number
# prediction_smote_with_class <- as.data.frame(scale_test) 
# prediction_smote_with_class <- mutate(prediction_smote_with_class, cluster = prediction_smote)
# prediction_smote_with_class <- mutate(prediction_smote_with_class, fraud = test$Class)
# ta2 <- table(prediction_smote_with_class$cluster, prediction_smote_with_class$fraud) 
# ta2 <- as.matrix(ta2)
# ta2 <- cbind(ta2, c(ta2[1,2]/ta2[1,1]*100, ta2[2,2]/ta2[2,1]*100, ta2[3,2]/ta2[3,1]*100, ta2[4,2]/ta2[4,1]*100, ta2[5,2]/ta2[5,1]*100, ta2[6,2]/ta2[6,1]*100))
# colnames(ta2)[1] = "Non-fraud"
# colnames(ta2)[2] = "Fraud"
# colnames(ta2)[3] = "Pct ratio"
# ta2

prediction_smote_3_with_class <- as.data.frame(scale_test)
prediction_smote_3_with_class <- mutate(prediction_smote_3_with_class, cluster = prediction_smote_3)
prediction_smote_3_with_class <- mutate(prediction_smote_3_with_class, fraud = test$Class)
ta4 <- table(prediction_smote_3_with_class$cluster, prediction_smote_3_with_class$fraud)
ta4 <- as.matrix(ta4)
#ta4 <- cbind(ta4, c(ta4[1,2]/ta4[1,1]*100, ta4[2,2]/ta4[2,1]*100, ta4[3,2]/ta4[3,1]*100))
colnames(ta4)[1] = "non_fraud"
colnames(ta4)[2] = "Fraud"
#colnames(ta4)[3] = "Pct ratio"
SMOTE_test_result <- ta4
SMOTE_test_result
SMOTE_test_result <- as.matrix(SMOTE_test_result)
round(prop.table(SMOTE_test_result,margin=1)*100, digits=2)


prediction_rose_with_class <- as.data.frame(scale_test) 
prediction_rose_with_class <- mutate(prediction_rose_with_class, cluster = prediction_rose)
prediction_rose_with_class <- mutate(prediction_rose_with_class, fraud = test$Class)
ta <- table(prediction_rose_with_class$cluster, prediction_rose_with_class$fraud) 
ta <- as.matrix(ta)
#ta <- cbind(ta, c(ta[1,2]/ta[1,1]*100, ta[2,2]/ta[2,1]*100, ta[3,2]/ta[3,1]*100, ta[4,2]/ta[4,1]*100))
colnames(ta)[1] = "non_fraud"
colnames(ta)[2] = "Fraud"
#colnames(ta)[3] = "Pct ratio"
ROSE_test_result<- ta
ROSE_test_result
ROSE_test_result <- as.matrix(ROSE_test_result)
round(prop.table(ROSE_test_result,margin=1)*100, digits=2)


#find confusion matrix
smote_test <- data.frame(prediction_smote_3_with_class$cluster, prediction_smote_3_with_class$fraud)
str(smote_test)
nf_clu <- smote_test$prediction_smote_3_with_class.cluster==3
smote_test_clu3 <- smote_test[nf_clu,]
smote_test_clu3 <- mutate(smote_test_clu3, predicted_class = 0)
smote_test_clu3
f_clu <- smote_test$prediction_smote_3_with_class.cluster!=3
smote_test_clu12 <- smote_test[f_clu,]
smote_test_clu12 <- mutate(smote_test_clu12, predicted_class = 1)
smote_test_clu <- data.frame(fraud_class=c(smote_test_clu3$prediction_smote_3_with_class.fraud, smote_test_clu12$prediction_smote_3_with_class.fraud), predicted_class=c(smote_test_clu3$predicted_class, smote_test_clu12$predicted_class))
smote_test_clu$fraud_class <- factor(smote_test_clu$fraud_class)
smote_test_clu$predicted_class <- factor(smote_test_clu$predicted_class)
#Creating confusion matrix
smote_matrix <- confusionMatrix(data=smote_test_clu$predicted_class, reference = smote_test_clu$fraud_class, mode = "everything", positive="1")
smote_matrix


rose_test <- data.frame(prediction_rose_with_class$cluster, prediction_rose_with_class$fraud)
str(rose_test)
nf_clu_r <- rose_test$prediction_rose_with_class.cluster==3
rose_test_clu3 <- rose_test[nf_clu_r,]
rose_test_clu3 <- mutate(rose_test_clu3, predicted_class = 0)

f_clu_r <- rose_test$prediction_rose_with_class.cluster!=3
rose_test_clu124 <- rose_test[f_clu_r,]
rose_test_clu124 <- mutate(rose_test_clu124, predicted_class = 1)
rose_test_clu <- data.frame(fraud_class=c(rose_test_clu3$prediction_rose_with_class.fraud, rose_test_clu124$prediction_rose_with_class.fraud), predicted_class=c(rose_test_clu3$predicted_class, rose_test_clu124$predicted_class))
rose_test_clu$fraud_class <- factor(rose_test_clu$fraud_class)
rose_test_clu$predicted_class <- factor(rose_test_clu$predicted_class)
#Creating confusion matrix
rose_matrix <- confusionMatrix(data=rose_test_clu$predicted_class, reference = rose_test_clu$fraud_class, mode = "everything", positive="1")
rose_matrix


