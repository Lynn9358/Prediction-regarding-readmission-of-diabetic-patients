######################################################################
# install.packages("rpart")
# install.packages("rpart.plot")
library(rpart) # decision tree
library(rpart.plot) #decision tree plot
library(MASS)
library(dplyr) # pipe functions
library(caret) # confusion matrix 
library(e1071) #
library(randomForest) # random forest

# transform readmitted to numeric
cl_data$readmitted <- as.numeric(as.character(cl_data$readmitted))

formula.model =  readmitted ~ factor(race) + factor(gender) + factor(age) + time_in_hospital + 
  num_lab_procedures + num_procedures + num_medications + number_outpatient + 
  number_emergency + number_inpatient + factor(repaglinide) + 
  factor(nateglinide) + factor(glimepiride) + factor(max_glu_serum) + 
  factor(A1Cresult) + factor(metformin) + factor(glipizide) + 
  factor(glyburide) + factor(pioglitazone) + factor(rosiglitazone) + 
  factor(acarbose) + factor(insulin) + change + diabetesMed + 
  factor(diag_recode)

#############split the data###########
set.seed(1)
split <- caTools::sample.split(cl_data, SplitRatio = 0.7) 
train <- subset(cl_data, split == "TRUE") 
test <- subset(cl_data, split == "FALSE") 

#######################logistic regression#######################
model_glm<-glm(formula.model ,family = binomial(link = 'logit'), data =train)
summary(model_glm)

glm.probs = predict(model_glm, newdata = test, type = "response")
glm.pred = ifelse(glm.probs > 0.5, "1", "0")



###########naive bayesian ############
nb_default <- naiveBayes(formula.model, data=train)
nb_probs <- predict(nb_default, test, type="raw")
nb_pred <- predict(nb_default, test, type="class")
# table(nb_pred)



###########decision tree#############
fit <- rpart(formula.model, data = train, method = 'class')
plot_rpart<- rpart.plot(fit, extra = 106)
rp_probs <- predict(fit, test, type="prob")
rp_pred <- predict(fit, test, type="class")
confusionMatrix(as.factor(rp_pred), as.factor(test$readmitted))

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

########random forest##########
x<-train[,-c(4:6,29)]
y<-train$readmitted

#define new observation
newx <- test[,-c(4:6,29)]

# model_rf500 <-randomForest(x,y,ntree=500,importance = TRUE,type="prob") #,maxnodes = 6,proximity = T
# plot(model_rf500)  #reason of choosing 100 trees

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
rf_probs_tuned<-predict(model_rftuned, newdata=newx, type="prob")
rf_pred_tuned<-factor(ifelse(rf_probs_tuned[,2]>0.5,1,0))
rf_tuned_matrix<-model_rftuned$confusion #confusion matrix
(rf_tuned_matrix[1,1]+rf_tuned_matrix[2,2])/sum(rf_tuned_matrix) #Accuracy

