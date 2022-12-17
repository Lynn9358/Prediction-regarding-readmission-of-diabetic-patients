diadata = read.csv("~/Downloads/FINAL/diabetic_data.csv")

#Recode for medical
Med = c(0,length = 24)
for (i in 1: 23) {
  Med[i]= length(diadata[,i+24][which(diadata[,i+24]=="Steady"|
                                        diadata[,i+24]=="Up"|
                                        diadata[,i+24]=="Down")])
}
names(Med) = colnames(diadata[,c(25:47)])
barplot(sort(Med,decreasing = TRUE))
head(sort(Med,decreasing = TRUE),n = 10)



##remove missing data in gender
diadata = diadata[diadata$gender != "Unknown/Invalid", ]
diadata = diadata[diadata$race != "?", ]

## giving data to readmission 
cl_data = subset(diadata,select = -c(encounter_id,patient_nbr,weight,payer_code, medical_specialty,examide,citoglipton,acetohexamide,chlorpropamide, acetohexamide,tolbutamide, miglitol,troglitazone,tolazamide, glipizide.metformin,glimepiride.pioglitazone,metformin.rosiglitazone,metformin.pioglitazone,glyburide.metformin))
cl_data$readmitted[cl_data$readmitted == "NO"] <- 0
cl_data$readmitted[cl_data$readmitted == ">30"] <- 1
cl_data$readmitted[cl_data$readmitted == "<30"] <- 1

## convert data in diag_1 to diag_3 start with V or E to 1000
cl_data$diag_1_recode <- cl_data$diag_1
cl_data$diag_1_recode[which(unlist(lapply("^(E|V)",grepl,cl_data$diag_1)))]<-"1000"
cl_data$diag_1_recode <- as.character(trunc(as.numeric(cl_data$diag_1_recode)))
#summary(as.factor(cl_data$diag_1_recode))

cl_data$diag_2_recode <-cl_data$diag_2
cl_data$diag_2_recode[which(unlist(lapply("^(E|V)",grepl,cl_data$diag_2)))]<-"1000"
cl_data$diag_2_recode <- as.character(trunc(as.numeric(cl_data$diag_2_recode)))
#summary(as.factor(cl_data$diag_2_recode))

cl_data$diag_3_recode <-cl_data$diag_3
cl_data$diag_3_recode[which(unlist(lapply("^(E|V)",grepl,cl_data$diag_3)))]<-"1000"
cl_data$diag_3_recode <- as.character(trunc(as.numeric(cl_data$diag_3_recode)))
#summary(as.factor(cl_data$diag_3_recode))

## unit value in diag
cl_data$diag_recode <- "Others"
cl_data$diag_recode[which(cl_data$diag_1_recode=="427"|
                            cl_data$diag_2_recode=="427"|
                            cl_data$diag_3_recode=="427")]<- "427"

cl_data$diag_recode[which(cl_data$diag_1_recode=="401"|
                            cl_data$diag_2_recode=="401"|
                            cl_data$diag_3_recode=="401")]<- "401"

cl_data$diag_recode[which(cl_data$diag_1_recode=="414"|
                            cl_data$diag_2_recode=="414"|
                            cl_data$diag_3_recode=="414")]<- "414"

cl_data$diag_recode[which(cl_data$diag_1_recode=="276"|
                            cl_data$diag_2_recode=="276"|
                            cl_data$diag_3_recode=="276")]<- "276"

cl_data$diag_recode[which(cl_data$diag_1_recode=="428"|
                            cl_data$diag_2_recode=="428"|
                            cl_data$diag_3_recode=="428")]<- "428"

cl_data$diag_recode[which(cl_data$diag_1_recode=="250"|
                            cl_data$diag_2_recode=="250"|
                            cl_data$diag_3_recode=="250")]<- "250"

cl_data = subset(cl_data,select = -c(diag_1,diag_2,diag_3,diag_1_recode,diag_2_recode,diag_3_recode))





##########################
# formula.model =  readmitted ~  factor(race)+  factor(gender)+   factor(age)+  factor(admission_type_id)+  factor(discharge_disposition_id)+ factor(admission_source_id)+ time_in_hospital + num_lab_procedures + num_procedures+ num_medications+ number_outpatient+ number_emergency + number_inpatient 
formula.model =  readmitted ~ factor(race) + gender + factor(age) + time_in_hospital + 
  num_lab_procedures + num_procedures + num_medications + number_outpatient + 
  number_emergency + number_inpatient + factor(repaglinide) + 
  factor(nateglinide) + factor(glimepiride) + factor(max_glu_serum) + 
  factor(A1Cresult) + factor(metformin) + factor(glipizide) + 
  factor(glyburide) + factor(pioglitazone) + factor(rosiglitazone) + 
  factor(acarbose) + factor(insulin) + change + diabetesMed + 
  factor(diag_recode) 

## must do:transform readmitted to numeric
cl_data$readmitted <- as.numeric(as.character(cl_data$readmitted))


library(MASS)
library(dplyr)
# model_AIC <- glm(formula.model, data = cl_data, family = binomial) %>%
#   stepAIC(trace = FALSE)
# model_BIC <-glm(formula.model, data = cl_data, family = binomial) %>%
#   stepAIC(trace = FALSE,k = log(nrow(cl_data)) )
# formula_AIC<-formula(model_AIC)
# formula_BIC<-formula(model_BIC)
# fit_step<-lm(formula_step,data = cl_data)
# summary(fit_step)

###age to continuous

#############ridge regression#############
x <- data.matrix(cl_data[,-cl_data$readmitted])
y <- cl_data$readmitted
#fit ridge regression model
library(glmnet)
model_rr <- glmnet(x,y, alpha = 0)
#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 0)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda

plot(cv_model) 
#find coefficients of best model
best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda)
coef(best_model)

#produce Ridge trace plot
plot(model_rr, xvar = "lambda")


#lasso
#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda
plot(cv_model) 


#############split the data###########
set.seed(1)
split <- caTools::sample.split(cl_data, SplitRatio = 0.7) 
train <- subset(cl_data, split == "TRUE") 
test <- subset(cl_data, split == "FALSE") 
######
model_glm<-glm(formula.model ,family = binomial(link = 'logit'), data = train)
summary(model_glm) #AIC: 91107
glm_probs = predict(model_glm, newdata = test, type = "response")
# glm_probs = predict(model_AIC, newdata = test, type = "response")
# glm_probs = predict(model_BIC, newdata = test, type = "response")

glm_pred = ifelse(glm_probs > 0.5, "1", "0")
# install.packages("Metrics")
# Metrics::rmse(as.numeric(test$readmitted),as.numeric(glm_pred)) #  0.6204811
# AIC model:  BIC model:

#######Confusion Matrix and ROC##########
library(caret)
confusionMatrix(as.factor(glm_pred), as.factor(test$readmitted))
pROC::auc(test$readmitted,glm_probs) #AUC
roc_glm<- pROC::roc(test$readmitted,glm_probs) 
plot(roc_glm)

###########naive bayesian ############
library(e1071)
#Default Paramters
set.seed(1)
nb_default <- naiveBayes(formula.model, data=train)
#naive_bayes(formula.model, data=cl_data,usekernel = T)
nb_probs <- predict(nb_default, test, type="raw")
nb_pred <- predict(nb_default, test, type="class")
# table(nb_pred)
# Metrics::rmse(as.numeric(test$readmitted),as.numeric(nb_pred)) # 0.879095
confusionMatrix(as.factor(nb_pred), as.factor(test$readmitted))
pROC::auc(test$readmitted,nb_probs[,2]) #AUC
roc_nb<- pROC::roc(test$readmitted,nb_probs[,2]) 
plot(roc_nb)

#########random forest##########
library(randomForest)
x<-train[,-c(4:6,29)] # x<-train[,-c(4:6,13,29)]
y<-as.factor(train$readmitted)
#define new observation
newx <- test[,-c(4:6, 29)]# test[,-c(4:6,13,29)]

# model_rf <-randomForest(readmitted~.,data = train[,-c(4,5,6)])
set.seed(1)
# model_rf <-randomForest(x,y,ntree=50,importance = TRUE,type="prob") #,maxnodes = 6,proximity = T
# # Grow no more than 4 nodes per tree
# model_rf500 <-randomForest(x,y,ntree=500,importance = TRUE,type="prob") #,maxnodes = 6,proximity = T

# #display fitted model
# model_rf
# model_rf500
# # OOB estimate of  error rate: 38.16%
# # Confusion matrix:
# #   0     1 class.error
# # 0 27312 10081   0.2695959
# # 1 16495 15757   0.5114412
# #display test MSE by number of trees
# plot(model_rf)
# plot(model_rf500)  #reason of choosing 100 trees
# 
# #produce variable importance plot
# varImpPlot(model_rf) 
# varImpPlot(model_rf500) 

#tune model
# model_tuned <- tuneRF(
#   x,
#   y,
#   ntreeTry=500,
#   stepFactor=1.5,
#   improve=0.01,
#   trace=FALSE #don't show real-time progress
# )
# mtry  OOBError
# 4.OOB    4 0.3810467
# 5.OOB    5 0.3822959
# 7.OOB    7 0.3843636

# rf_pred_500<-predict(model_rf500, newdata=newx, type="prob")
# pROC::auc(test$readmitted,as.numeric(rf_pred_500[,2])) #AUC
# 
# roc_rf500<- pROC::roc(test$readmitted,as.numeric(rf_pred_500[,2])) 
# plot(roc_rf500)
# 
# #example:
# cbind(as.numeric(rf_pred_500[,2]),test[,-c(4:6,29)])[1,]


# set.seed(1)
# model_tuned2 <- tuneRF(
#   x,
#   y,
#   ntreeTry=200,
#   stepFactor=1.5,
#   improve=0.01,
#   trace=FALSE #don't show real-time progress
# )

#       mtry  OOBError
# 4.OOB    4 0.3835020
# 5.OOB    5 0.3853974
# 7.OOB    7 0.3885275

# tune the parameters
set.seed(1)
model_tuned1 <- tuneRF(
  x,
  y,
  ntreeTry=100,
  stepFactor=1.5,
  improve=0.01,
  trace=FALSE #don't show real-time progress
)
#       mtry  OOBError
# 4.OOB    4 0.3869768
# 5.OOB    5 0.3892455
# 7.OOB    7 0.3933233


set.seed(1)
model_rftuned<-randomForest(x,y,ntree=100,importance = TRUE,mtry=4,type="prob")
varImpPlot(model_rftuned)
rf_probs_tuned<-predict(model_rftuned, newdata=newx, type="prob")
rf_pred_tuned<-factor(ifelse(rf_probs_tuned[,2]>0.5,1,0))
rf_tuned_matrix<-model_rftuned$confusion #confusion matrix
(rf_tuned_matrix[1,1]+rf_tuned_matrix[2,2])/sum(rf_tuned_matrix) #Accuracy=0.6121834

pROC::auc(test$readmitted,as.numeric(rf_probs_tuned[,2])) #AUC=0.6563
roc_rf_tuned<- pROC::roc(test$readmitted,rf_probs_tuned[,2]) 
plot(roc_rf_tuned)



###########decision tree#############
# install.packages("rpart")
# install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
fit <- rpart(formula.model, data = train, method = 'class')
plot_rpart<- rpart.plot(fit, extra = 106)
rp_probs <- predict(fit, test, type="prob")
rp_pred <- predict(fit, test, type="class")
confusionMatrix(as.factor(rp_pred), as.factor(test$readmitted))
pROC::auc(test$readmitted,rp_probs[,2]) #AUC
roc_rp<- pROC::roc(test$readmitted,rp_probs[,2]) 
plot(roc_rp)

# tune the parameters
# accuracy should be higher than 0.78
control_rpart <- rpart.control(minsplit = 4,
                         minbucket = round(5 / 3),
                         maxdepth = 3,
                         cp = 0)
set.seed(1)
tune_rpart <- rpart(formula.model, data = train, method = 'class', control = control_rpart)
tune_rpart$variable.importance #importance of variables

rpart.plot(tune_rpart, extra = 106) #final results-- decision tree
# predictions
tune_rp_probs <- predict(tune_rpart, test, type="prob")
tune_rp_pred <- predict(tune_rpart, test, type="class")
unique(tune_rp_probs) # 4 possibilities
#              0         1
# 4    0.6237536 0.3762464
# 48   0.3881849 0.6118151
# 674  0.4521132 0.5478868
# 3084 0.5510836 0.4489164

confusionMatrix(as.factor(tune_rp_pred), as.factor(test$readmitted))
# Confusion Matrix and Statistics
# 
# Reference
# Prediction     0     1
# 0 11571  7134
# 1  4352  6790

pROC::auc(test$readmitted,tune_rp_probs[,2]) #AUC
roc_tune_rp<- pROC::roc(test$readmitted,tune_rp_probs[,2]) 
plot(roc_tune_rp)

##################knn##################

library(caret)
library(class)

xx<-as.matrix(model.matrix(formula.model,data = train))[,-1]
newxx<-as.matrix(model.matrix(formula.model,data = test))[,-1]


model_knn_3<-knn(xx,newxx, train$readmitted, k = 3, prob=TRUE)
model_knn_5<-knn(train, test, train$readmitted, k = 5, prob=TRUE)

# 10-folds cross validation
control_knn <- trainControl(method = 'cv',number = 10)
# model
model_knn <- train(formula.model,train,
               method = 'knn',
               preProcess = c('center','scale'),
               trControl = control_knn,
               tuneGrid = data.frame(k=3)
               ) #standardized # # tuneLength = 5
model_knn
pred <- predict(model_knn,newdata = newx)
# confusion matrix
confusionMatrix(as.factor(tune_rp_pred), as.factor(test$readmitted))



#############results##########
# confusion matrix
# logistic
glmcm <- confusionMatrix(as.factor(glm_pred), as.factor(test$readmitted))
glmaccuracy <- glmcm$overall[1]
pltcm <- as.data.frame(glmcm$table)
pltcm$Prediction <- factor(pltcm$Prediction, levels=rev(levels(pltcm$Prediction)))
pltcm_gg_glm <- ggplot(pltcm, aes(Prediction,Reference, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction")+ggtitle("Logistic Regression")+
  theme(plot.title = element_text(hjust = 0.5))
pltcm_gg_glm
# ggsave("pltcm_gg_glm.png")


# naive bayesian
nbcm <- confusionMatrix(as.factor(nb_pred), as.factor(test$readmitted))
nbaccuracy <- nbcm$overall[1]
pltcm <- as.data.frame(glmcm$table)
pltcm$Prediction <- factor(pltcm$Prediction, levels=rev(levels(pltcm$Prediction)))
pltcm_gg_nb <- ggplot(pltcm, aes(Prediction,Reference, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction")+ggtitle("Naive Bayesian")+
  theme(plot.title = element_text(hjust = 0.5))
pltcm_gg_nb
# ggsave("pltcm_gg_nb.png")


# decision tree
rpcm <- confusionMatrix(as.factor(rp_pred), as.factor(test$readmitted))
rpaccuracy <- rpcm$overall[1]
pltcm <- as.data.frame(rpcm$table)

pltcm$Prediction <- factor(pltcm$Prediction, levels=rev(levels(pltcm$Prediction)))
pltcm_gg_rp <- ggplot(pltcm, aes(Prediction,Reference, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction")+ggtitle("Decision Tree")+
  theme(plot.title = element_text(hjust = 0.5))
pltcm_gg_rp
# ggsave("pltcm_gg_rp.png")

# random forest

rfcm <- confusionMatrix(as.factor(rf_pred), as.factor(test$readmitted))
rfaccuracy <- rfcm$overall[1]
pltcm <- as.data.frame(rfcm$table)

pltcm$Prediction <- factor(pltcm$Prediction, levels=rev(levels(pltcm$Prediction)))
pltcm_gg_rf <- ggplot(pltcm, aes(Prediction,Reference, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction")+ggtitle("Random Forest")+
  theme(plot.title = element_text(hjust = 0.5))
pltcm_gg_rf
# ggsave("pltcm_gg_rf.png")



# save images
png(filename = "pltcm_gg_glm.png", width = 516, height = 440,
    units = "px", pointsize = 12, bg = "white", res = NA
    )
pltcm_gg_glm
dev.off()
pltcm_gg_glm
# variable importance
ggplot(df, aes(x=age)) + 
  geom_histogram(color='black',fill="royalblue")+
  coord_flip()


png(filename = "Rplot%03d.png", width = 480, height = 480,
    units = "px", pointsize = 12, bg = "white", res = NA,
    restoreConsole = TRUE)

png()
plot(rnorm(100))
dev.off()
